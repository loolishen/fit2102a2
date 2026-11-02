module Assignment
    ( ADT (..)
    , Declaration (..)
    , Domain (..)
    , Expr (..)
    , Constraint (..)
    , CompareOperator (..)
    , SolverGoal (..)
    , MacroName (..)
    , MacroArgs (..)
    , CompMacroArgs (..)
    , miniZincParser
    , solveProblem
    , getTime
    ) where

import Control.Applicative ((<|>), many, some)
import Data.Char (isAlpha, isDigit)
import Data.List (maximumBy, minimumBy, nub)
import Data.Maybe (listToMaybe)
import Data.Ord (comparing)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)

import Instances (ParseError (..), Parser (..))
import Parser (char, eof, failed, is, satisfy, sepBy, string)

--------------------------------------------------------------------------------
-- AST
--------------------------------------------------------------------------------

type Identifier = String

data ADT = ADT
    { declarations :: [Declaration]
    , constraints  :: [Constraint]
    , solverGoal   :: SolverGoal
    }
    deriving (Eq, Show)

-- (AI-Assists) Part D: Added ArrayDeclaration to support array[1..n] of var
data Declaration
    = ConstantDeclaration Identifier Expr
    | VariableDeclaration Identifier Domain
    | ArrayDeclaration Identifier Expr Domain
    deriving (Eq, Show)

data Domain
    = Range Int Int
    | FiniteSet [Int]
    deriving (Eq, Show)

-- (AI-Assists) Part D: Macro arguments supporting literal lists, array identifiers,
-- and generator expressions like (i in 1..n)(q[i] + i)
data MacroArgs
    = ListArgs [Expr]
    | ArrayArg Identifier
    | GeneratorArg Identifier Domain Expr
    deriving (Eq, Show)

-- (AI-Assists) Part D: Boolean macro arguments for all/any
data CompMacroArgs
    = CompListArgs [Constraint]
    | CompArrayArg Identifier
    | CompGeneratorArg Identifier Domain Constraint
    deriving (Eq, Show)

-- (AI-Assists) Part D: Added ArrayElem for q[i] and updated MacroExpr for MacroArgs
data Expr
    = IntLit Int
    | Var Identifier
    | ArrayElem Identifier Expr
    | Neg Expr
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Pow Expr Expr
    | MacroExpr MacroName MacroArgs
    deriving (Eq, Show)

data CompareOperator = OpEq | OpNeq | OpLt | OpLte | OpGt | OpGte
    deriving (Eq, Show)

-- (AI-Assists) Part D: Updated macros to support the three argument syntaxes
data Constraint
    = Compare CompareOperator Expr Expr
    | All CompMacroArgs
    | Any CompMacroArgs
    | AllDifferent MacroArgs
    deriving (Eq, Show)

data MacroName = Sum | Min | Max
    deriving (Eq, Show)

data SolverGoal
    = Satisfy
    | Maximize Expr
    | Minimize Expr
    deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Parser
--------------------------------------------------------------------------------
data ParseEnv = ParseEnv
    { envConstants :: [(Identifier, Int)]
    , envVars      :: [Identifier]
    , envArrays    :: [(Identifier, Int)]
    , envGenVars   :: [Identifier]
    }

emptyEnv :: ParseEnv
emptyEnv = ParseEnv [] [] [] []

--  Ignore MiniZinc comments starting with '%' until the end of the line
comment :: Parser ()
comment = is '%' *> many (satisfy (/= '\n')) *> pure ()

--  Consume any mix of whitespace characters, newlines, and comments
whiteSpace :: Parser ()
whiteSpace =
    many (satisfy (`elem` " \t\r\n\f\v") *> pure () <|> comment) *> pure ()

-- Combinator to parse a token and consume trailing whitespace
token :: Parser a -> Parser a
token p = p <* whiteSpace

tokenChar :: Char -> Parser Char
tokenChar = token . char

tokenString :: String -> Parser String
tokenString = token . string

reservedKeywords :: [String]
reservedKeywords =
    [ "int", "var", "array", "of", "constraint", "solve"
    , "satisfy", "maximize", "minimize", "sum", "min", "max"
    , "all", "any", "allDifferent", "in"
    ]

-- (AI-Assists) Parse a keyword ensuring it is not a prefix of an identifier
keyword :: String -> Parser String
keyword kw = token $ do
    s <- string kw
    let isIdent c = isAlpha c || isDigit c || c == '_'
    hasTrailing <- (satisfy isIdent *> pure True) <|> pure False
    if hasTrailing
        then failed (UnexpectedString ("Keyword prefix collision: " ++ kw))
        else pure s

-- (AI-Assists) Parse an identifier starting with a letter
rawIdentifier :: Parser Identifier
rawIdentifier = token $ do
    first <- satisfy isAlpha
    rest  <- many (satisfy (\c -> isAlpha c || isDigit c || c == '_'))
    let name = first : rest
    if name `elem` reservedKeywords
        then failed (UnexpectedString ("Reserved keyword: " ++ name))
        else pure name

-- (AI-Assists) Parse integer literals, including optional leading negative signs
pIntLiteral :: Parser Int
pIntLiteral = token $ do
    sign   <- (char '-' *> pure (-1)) <|> pure 1
    digits <- some (satisfy isDigit)
    pure (sign * read digits)

-- (AI-Assists) Evaluate a constant expression purely using known constants
evalConstExpr :: [(Identifier, Int)] -> Expr -> Maybe Int
evalConstExpr consts expr = case expr of
    IntLit n         -> Just n
    Var x            -> lookup x consts
    ArrayElem _ _    -> Nothing
    Neg e            -> negate <$> evalConstExpr consts e
    Add a b          -> (+) <$> evalConstExpr consts a
                            <*> evalConstExpr consts b
    Sub a b          -> (-) <$> evalConstExpr consts a
                            <*> evalConstExpr consts b
    Mul a b          -> (*) <$> evalConstExpr consts a
                            <*> evalConstExpr consts b
    Div a b          -> do
        va <- evalConstExpr consts a
        vb <- evalConstExpr consts b
        if vb == 0 then Nothing else Just (va `div` vb)
    Pow a b          -> do
        va <- evalConstExpr consts a
        vb <- evalConstExpr consts b
        if vb < 0 then Nothing else Just (va ^ vb)
    MacroExpr _ _    -> Nothing

--------------------------------------------------------------------------------
-- Part A: Domain Parsing
--------------------------------------------------------------------------------
pDomain :: ParseEnv -> Parser Domain
pDomain env = pRange <|> pFiniteSet
  where
    pRange = do
        loE <- pExpr env
        _   <- tokenString ".."
        hiE <- pExpr env
        case ( evalConstExpr (envConstants env) loE
             , evalConstExpr (envConstants env) hiE
             ) of
            (Just lo, Just hi)
                | lo <= hi  -> pure (Range lo hi)
                | otherwise -> failed
                    (UnexpectedString "Range lower bound > upper bound")
            _ -> failed
                (UnexpectedString "Range bounds must be known constants")

    pFiniteSet = do
        _  <- tokenChar '{'
        xs <- sepBy pIntLiteral (tokenChar ',')
        _  <- tokenChar '}'
        if length xs == length (nub xs)
            then pure (FiniteSet xs)
            else failed (UnexpectedString "Duplicate values in finite set")

--------------------------------------------------------------------------------
-- Part A: Expression Parsing (Operator Precedence Climbing)
--------------------------------------------------------------------------------

--  Parse comparison operators.
pCompareOp :: Parser CompareOperator
pCompareOp = (tokenString "!=" *> pure OpNeq)
         <|> (tokenString "<=" *> pure OpLte)
         <|> (tokenString ">=" *> pure OpGte)
         <|> (tokenString "="  *> pure OpEq)
         <|> (tokenString "<"  *> pure OpLt)
         <|> (tokenString ">"  *> pure OpGt)

-- (AI assists) Tier 1: Addition and Subtraction (lowest precedence).
pExpr :: ParseEnv -> Parser Expr
pExpr env = pMul >>= restAdd
  where
    restAdd acc = (tokenChar '+' *> pMul >>= \r -> restAdd (Add acc r))
              <|> (tokenChar '-' *> pMul >>= \r -> restAdd (Sub acc r))
              <|> pure acc

    -- Tier 2: Multiplication and Division
    pMul = pUnary >>= restMul
    restMul acc = (tokenChar '*' *> pUnary >>= \r -> restMul (Mul acc r))
              <|> (tokenChar '/' *> pUnary >>= \r -> restMul (Div acc r))
              <|> pure acc

    -- Tier 3: Unary minus (binds tighter than * and /)
    pUnary = (tokenChar '-' *> (Neg <$> pUnary)) <|> pPower

    -- Tier 4: Right-associative exponentiation (^)
    pPower = do
        a <- pAtom
        (tokenChar '^' *> (Pow a <$> pPower)) <|> pure a

    -- Tier 5: Primary atoms
    pAtom = (tokenChar '(' *> pExpr env <* tokenChar ')')
        <|> pNumericMacroExpr env
        <|> pVarOrArrayElem env
        <|> (IntLit <$> pIntLiteral)

-- (AI-Assists) Part D: Parse variable or array element access (e.g. q[i])
pVarOrArrayElem :: ParseEnv -> Parser Expr
pVarOrArrayElem env = do
    name <- rawIdentifier
    let isConst = name `elem` map fst (envConstants env)
        isVar   = name `elem` envVars env
        isArr   = name `elem` map fst (envArrays env)
        isGen   = name `elem` envGenVars env
    hasIndex <- (tokenChar '[' *> pure True) <|> pure False
    if hasIndex
        then do
            idx <- pExpr env <* tokenChar ']'
            if isArr
                then pure (ArrayElem name idx)
                else failed (UnexpectedString ("Not an array: " ++ name))
        else do
            if isConst || isVar || isGen
                then pure (Var name)
                else failed (UnexpectedString ("Undefined variable: " ++ name))

-- (AI-Assists) Part D: Parse numeric macros supporting literal, array, and generator forms
pNumericMacroExpr :: ParseEnv -> Parser Expr
pNumericMacroExpr env = do
    macro <- (keyword "sum" *> pure Sum)
         <|> (keyword "min" *> pure Min)
         <|> (keyword "max" *> pure Max)
    args  <- pMacroArgs env
    pure (MacroExpr macro args)

-- (AI-Assists) Part D: Parse macro arguments in literal, array, or generator syntax
pMacroArgs :: ParseEnv -> Parser MacroArgs
pMacroArgs env = pLiteralList <|> pGenOrArray
  where
    pLiteralList = do
        _  <- tokenChar '(' *> tokenChar '['
        es <- sepBy (pExpr env) (tokenChar ',')
        _  <- tokenChar ']' <* tokenChar ')'
        pure (ListArgs es)

    pGenOrArray = do
        _     <- tokenChar '('
        name  <- rawIdentifier
        isGen <- (keyword "in" *> pure True) <|> pure False
        if isGen
            then do
                dom   <- pDomain env
                _     <- tokenChar ')' *> tokenChar '('
                let env' = env { envGenVars = name : envGenVars env }
                body  <- pExpr env' <* tokenChar ')'
                pure (GeneratorArg name dom body)
            else do
                _ <- tokenChar ')'
                if name `elem` map fst (envArrays env)
                    then pure (ArrayArg name)
                    else failed (UnexpectedString ("Expected array: " ++ name))

-- (AI-Assists) Part D: Parse boolean macro arguments (all, any)
pCompMacroArgs :: ParseEnv -> Parser CompMacroArgs
pCompMacroArgs env = pLiteralList <|> pGenOrArray
  where
    pLiteralList = do
        _     <- tokenChar '(' *> tokenChar '['
        comps <- sepBy (pCompItem env) (tokenChar ',')
        _     <- tokenChar ']' <* tokenChar ')'
        pure (CompListArgs comps)

    pGenOrArray = do
        _     <- tokenChar '('
        name  <- rawIdentifier
        isGen <- (keyword "in" *> pure True) <|> pure False
        if isGen
            then do
                dom   <- pDomain env
                _     <- tokenChar ')' *> tokenChar '('
                let env' = env { envGenVars = name : envGenVars env }
                body  <- pCompItem env' <* tokenChar ')'
                pure (CompGeneratorArg name dom body)
            else do
                _ <- tokenChar ')'
                if name `elem` map fst (envArrays env)
                    then pure (CompArrayArg name)
                    else failed (UnexpectedString ("Expected array: " ++ name))

-- Parse a single comparison: <expr> <op> <expr>
pCompItem :: ParseEnv -> Parser Constraint
pCompItem env = do
    e1 <- pExpr env
    op <- pCompareOp
    e2 <- pExpr env
    pure (Compare op e1 e2)

--------------------------------------------------------------------------------
-- Part A & D: Declarations, Constraints & Model Entry Point
--------------------------------------------------------------------------------

-- (AI assists) Parse constraint statements including boolean macros.
pConstraint :: ParseEnv -> Parser Constraint
pConstraint env = keyword "constraint" *> pBody <* tokenChar ';'
  where
    pBody = (keyword "allDifferent" *> (AllDifferent <$> pMacroArgs env))
        <|> (keyword "all"          *> (All <$> pCompMacroArgs env))
        <|> (keyword "any"          *> (Any <$> pCompMacroArgs env))
        <|> pCompItem env

--  Parse solve directives: satisfy, maximize, minimize
pSolveDirective :: ParseEnv -> Parser SolverGoal
pSolveDirective env = keyword "solve" *> pGoal <* tokenChar ';'
  where
    pGoal = (keyword "satisfy"  *> pure Satisfy)
        <|> (keyword "maximize" *> (Maximize <$> pExpr env))
        <|> (keyword "minimize" *> (Minimize <$> pExpr env))

--  Ensure variable or constant name has not already been declared
checkUnique :: Identifier -> ParseEnv -> Parser ()
checkUnique name env =
    if name `elem` ( map fst (envConstants env)
                  ++ envVars env
                  ++ map fst (envArrays env)
                   )
        then failed (UnexpectedString ("Duplicate identifier: " ++ name))
        else pure ()

--  Parse: int: <identifier> = <expression>;
pConstantDecl :: ParseEnv -> Parser (Declaration, ParseEnv)
pConstantDecl env = do
    _    <- keyword "int" *> tokenChar ':'
    name <- rawIdentifier
    checkUnique name env
    _    <- tokenChar '='
    e    <- pExpr env
    _    <- tokenChar ';'
    case evalConstExpr (envConstants env) e of
        Just val -> do
            let env' = env { envConstants = (name, val) : envConstants env }
            pure (ConstantDeclaration name e, env')
        Nothing  -> failed
            (UnexpectedString ("Constant expression cannot be resolved: " ++ name))

--  Parse: var <domain>: <identifier>;
pVariableDecl :: ParseEnv -> Parser (Declaration, ParseEnv)
pVariableDecl env = do
    _    <- keyword "var"
    dom  <- pDomain env
    _    <- tokenChar ':'
    name <- rawIdentifier
    checkUnique name env
    _    <- tokenChar ';'
    let env' = env { envVars = name : envVars env }
    pure (VariableDeclaration name dom, env')

-- (AI-Assists) Part D: Parse array declaration:
-- array[1..<len>] of var <domain>: <identifier>;
pArrayDecl :: ParseEnv -> Parser (Declaration, ParseEnv)
pArrayDecl env = do
    _    <- keyword "array" *> tokenChar '['
    _    <- tokenChar '1' *> tokenString ".."
    lenE <- pExpr env
    _    <- tokenChar ']' *> keyword "of" *> keyword "var"
    dom  <- pDomain env
    _    <- tokenChar ':'
    name <- rawIdentifier
    checkUnique name env
    _    <- tokenChar ';'
    case evalConstExpr (envConstants env) lenE of
        Just lenVal
            | lenVal >= 1 -> do
                let env' = env { envArrays = (name, lenVal) : envArrays env }
                pure (ArrayDeclaration name lenE dom, env')
            | otherwise   -> failed
                (UnexpectedString "Array length must be at least 1")
        Nothing -> failed
            (UnexpectedString "Cannot evaluate array length as constant")

data Statement
    = DeclStmt Declaration ParseEnv
    | ConstrStmt Constraint

pStatement :: ParseEnv -> Parser Statement
pStatement env = (uncurry DeclStmt <$> pConstantDecl env)
             <|> (uncurry DeclStmt <$> pArrayDecl env)
             <|> (uncurry DeclStmt <$> pVariableDecl env)
             <|> (ConstrStmt <$> pConstraint env)

pStatements :: ParseEnv -> Parser ([Declaration], [Constraint], ParseEnv)
pStatements env = (do
    stmt <- pStatement env
    case stmt of
        DeclStmt d env' -> do
            (ds, cs, finalEnv) <- pStatements env'
            pure (d : ds, cs, finalEnv)
        ConstrStmt c -> do
            (ds, cs, finalEnv) <- pStatements env
            pure (ds, c : cs, finalEnv)
    ) <|> pure ([], [], env)

--  Complete MiniZinc parser consuming the entire file.
miniZincParser :: Parser ADT
miniZincParser = do
    whiteSpace
    (decls, constrs, env) <- pStatements emptyEnv
    goal <- pSolveDirective env
    whiteSpace
    eof
    pure (ADT decls constrs goal)

--------------------------------------------------------------------------------
-- Part B & D: Incremental Constraint Solver & Array Flattening
--------------------------------------------------------------------------------

type Binding = (Identifier, Int)

-- | (AI-Assisted) Three-valued evaluation state to support incremental pruning.
data EvalResult a
    = EvalOk a
    | Unassigned
    | EvalError
    deriving (Eq, Show)

instance Functor EvalResult where
    fmap f (EvalOk x)  = EvalOk (f x)
    fmap _ Unassigned  = Unassigned
    fmap _ EvalError   = EvalError

instance Applicative EvalResult where
    pure = EvalOk
    EvalOk f <*> EvalOk x = EvalOk (f x)
    EvalError <*> _       = EvalError
    _ <*> EvalError       = EvalError
    _ <*> _               = Unassigned

domainValues :: Domain -> [Int]
domainValues (Range lo hi)  = [lo..hi]
domainValues (FiniteSet xs) = xs

-- (AI-Assists) Part D: Substitute generator variable with concrete integer
substExpr :: Identifier -> Int -> Expr -> Expr
substExpr v val expr = case expr of
    IntLit n         -> IntLit n
    Var x            -> if x == v then IntLit val else Var x
    ArrayElem a idx  -> ArrayElem a (substExpr v val idx)
    Neg e            -> Neg (substExpr v val e)
    Add a b          -> Add (substExpr v val a) (substExpr v val b)
    Sub a b          -> Sub (substExpr v val a) (substExpr v val b)
    Mul a b          -> Mul (substExpr v val a) (substExpr v val b)
    Div a b          -> Div (substExpr v val a) (substExpr v val b)
    Pow a b          -> Pow (substExpr v val a) (substExpr v val b)
    MacroExpr m args -> MacroExpr m (substArgs v val args)

-- (AI-Assists) Part D: Substitute in macro arguments
substArgs :: Identifier -> Int -> MacroArgs -> MacroArgs
substArgs v val args = case args of
    ListArgs es        -> ListArgs (map (substExpr v val) es)
    ArrayArg a         -> ArrayArg a
    GeneratorArg i d b -> if i == v
                              then GeneratorArg i d b
                              else GeneratorArg i d (substExpr v val b)

-- (AI-Assists) Part D: Substitute in constraints
substConstraint :: Identifier -> Int -> Constraint -> Constraint
substConstraint v val constraint = case constraint of
    Compare op a b -> Compare op (substExpr v val a) (substExpr v val b)
    AllDifferent args -> AllDifferent (substArgs v val args)
    All (CompListArgs cs) ->
        All (CompListArgs (map (substConstraint v val) cs))
    All (CompGeneratorArg i d c) ->
        if i == v
            then All (CompGeneratorArg i d c)
            else All (CompGeneratorArg i d (substConstraint v val c))
    All (CompArrayArg a) -> All (CompArrayArg a)
    Any (CompListArgs cs) ->
        Any (CompListArgs (map (substConstraint v val) cs))
    Any (CompGeneratorArg i d c) ->
        if i == v
            then Any (CompGeneratorArg i d c)
            else Any (CompGeneratorArg i d (substConstraint v val c))
    Any (CompArrayArg a) -> Any (CompArrayArg a)

-- (AI-Assists) Part D: Expand array form or generator macro into expression list
expandMacroArgs :: [(Identifier, Int)] -> MacroArgs -> [Expr]
expandMacroArgs arrs args = case args of
    ListArgs es -> es
    ArrayArg a  ->
        let len = maybe 0 id (lookup a arrs)
        in [ArrayElem a (IntLit i) | i <- [1..len]]
    GeneratorArg i dom body ->
        [substExpr i val body | val <- domainValues dom]

-- (AI-Assists) Part D: Expand constraints over arrays and generators
expandConstraint :: [(Identifier, Int)] -> Constraint -> [Constraint]
expandConstraint arrs c = case c of
    Compare op a b ->
        [Compare op (expandExpr arrs a) (expandExpr arrs b)]
    AllDifferent args ->
        [AllDifferent (ListArgs (expandMacroArgs arrs args))]
    All (CompListArgs cs) ->
        concatMap (expandConstraint arrs) cs
    All (CompArrayArg _) -> []
    All (CompGeneratorArg i dom body) ->
        concatMap (expandConstraint arrs)
                  [substConstraint i val body | val <- domainValues dom]
    Any (CompListArgs cs) ->
        [Any (CompListArgs (concatMap (expandConstraint arrs) cs))]
    Any (CompArrayArg _) -> []
    Any (CompGeneratorArg i dom body) ->
        [Any (CompListArgs [substConstraint i v b | v <- domainValues dom])]
      where
        b = body

-- (AI-Assists) Part D: Expand expressions with macros
expandExpr :: [(Identifier, Int)] -> Expr -> Expr
expandExpr arrs expr = case expr of
    IntLit n         -> IntLit n
    Var x            -> Var x
    ArrayElem a idx  -> ArrayElem a (expandExpr arrs idx)
    Neg e            -> Neg (expandExpr arrs e)
    Add a b          -> Add (expandExpr arrs a) (expandExpr arrs b)
    Sub a b          -> Sub (expandExpr arrs a) (expandExpr arrs b)
    Mul a b          -> Mul (expandExpr arrs a) (expandExpr arrs b)
    Div a b          -> Div (expandExpr arrs a) (expandExpr arrs b)
    Pow a b          -> Pow (expandExpr arrs a) (expandExpr arrs b)
    MacroExpr m args -> MacroExpr m (ListArgs (expandMacroArgs arrs args))

-- (AI-Assists) Evaluate an expression under partial assignments (Part B & D)
evalExprVal :: [(Identifier, Int)]
            -> [(Identifier, Int)]
            -> [Binding]
            -> Expr
            -> EvalResult Int
evalExprVal consts arrs solved expr = case expr of
    IntLit n -> EvalOk n
    Var x    ->
        case lookup x consts of
            Just v  -> EvalOk v
            Nothing -> case lookup x solved of
                Just v  -> EvalOk v
                Nothing -> Unassigned
    -- (AI-Assists) Part D: Array indexing lookup with bounds checking
    ArrayElem a idxE ->
        case evalExprVal consts arrs solved idxE of
            EvalOk idx ->
                let len = maybe 0 id (lookup a arrs)
                in if idx < 1 || idx > len
                       then EvalError
                       else case lookup (a ++ "[" ++ show idx ++ "]") solved of
                           Just v  -> EvalOk v
                           Nothing -> Unassigned
            Unassigned -> Unassigned
            EvalError  -> EvalError
    Neg e    -> negate <$> evalExprVal consts arrs solved e
    Add a b  -> (+) <$> evalExprVal consts arrs solved a
                    <*> evalExprVal consts arrs solved b
    Sub a b  -> (-) <$> evalExprVal consts arrs solved a
                    <*> evalExprVal consts arrs solved b
    Mul a b  -> (*) <$> evalExprVal consts arrs solved a
                    <*> evalExprVal consts arrs solved b
    Div a b  -> do
        va <- evalExprVal consts arrs solved a
        vb <- evalExprVal consts arrs solved b
        if vb == 0 then EvalError else EvalOk (va `div` vb)
    Pow a b  -> do
        va <- evalExprVal consts arrs solved a
        vb <- evalExprVal consts arrs solved b
        if vb < 0 then EvalError else EvalOk (va ^ vb)
    MacroExpr m (ListArgs es) -> evalMacro m es
    MacroExpr m args          -> evalMacro m (expandMacroArgs arrs args)
  where
    evalMacro m es = do
        vs <- traverse (evalExprVal consts arrs solved) es
        case m of
            Sum -> pure (sum vs)
            Min -> if null vs then EvalError else pure (minimum vs)
            Max -> if null vs then EvalError else pure (maximum vs)

applyCompare :: CompareOperator -> Int -> Int -> Bool
applyCompare OpEq  = (==)
applyCompare OpNeq = (/=)
applyCompare OpLt  = (<)
applyCompare OpLte = (<=)
applyCompare OpGt  = (>)
applyCompare OpGte = (>=)

-- | (AI-Assisted) Incrementally check constraints during backtracking.
evalConstraintIncremental :: [(Identifier, Int)]
                          -> [(Identifier, Int)]
                          -> [Binding]
                          -> Constraint
                          -> EvalResult Bool
evalConstraintIncremental consts arrs solved constraint =
    case constraint of
        Compare op a b ->
            applyCompare op <$> evalExprVal consts arrs solved a
                            <*> evalExprVal consts arrs solved b
        AllDifferent (ListArgs es) ->
            let results = map (evalExprVal consts arrs solved) es
            in if any (== EvalError) results
                   then EvalOk False
                   else let okVals = [v | EvalOk v <- results]
                        in if length okVals /= length (nub okVals)
                               -- Duplicate detected among assigned variables
                               then EvalOk False
                               else if any (== Unassigned) results
                                        then Unassigned
                                        else EvalOk True
        AllDifferent args ->
            evalConstraintIncremental consts arrs solved
                (AllDifferent (ListArgs (expandMacroArgs arrs args)))
        All (CompListArgs cs) ->
            let results =
                    map (evalConstraintIncremental consts arrs solved) cs
            in if any (== EvalOk False) results || any (== EvalError) results
                   then EvalOk False
                   else if any (== Unassigned) results
                            then Unassigned
                            else EvalOk True
        All (CompArrayArg _) -> EvalOk True
        All (CompGeneratorArg _ _ _) -> EvalOk True
        Any (CompListArgs cs) ->
            let results =
                    map (evalConstraintIncremental consts arrs solved) cs
            in if any (== EvalOk True) results
                   then EvalOk True
                   else if all (== EvalOk False) results
                            then EvalOk False
                            else Unassigned
        Any (CompArrayArg _) -> EvalOk True
        Any (CompGeneratorArg _ _ _) -> EvalOk True

-- | (AI-Assisted) Backtracking search engine with early failure detection.
searchWithPruning :: [(Identifier, Int)]
                  -> [(Identifier, Int)]
                  -> [(Identifier, [Int])]
                  -> [Constraint]
                  -> [Binding]
                  -> [[Binding]]
searchWithPruning consts arrs remainingVars constrs currentSolved =
    case remainingVars of
        [] -> [reverse currentSolved]
        (v, dom) : rest -> do
            val <- dom
            let nextSolved = (v, val) : currentSolved
                evals = map
                    (evalConstraintIncremental consts arrs nextSolved)
                    constrs
            -- Prune search immediately if an evaluable constraint failed
            if any (== EvalOk False) evals
                then []
                else searchWithPruning consts arrs rest constrs nextSolved

-- | (AI-Assisted) Solve the problem and satisfy or optimize the goal.
solveProblem :: ADT -> Maybe [(String, Int)]
solveProblem adt =
    let (consts, arrs, decVars) = processDeclarations (declarations adt)
        allExpandedConstrs      = concatMap
            (expandConstraint arrs)
            (constraints adt)
        solutions               = searchWithPruning
            consts
            arrs
            decVars
            allExpandedConstrs
            []
    in case solverGoal adt of
        Satisfy -> listToMaybe solutions
        Minimize objE ->
            let expandedObj   = expandExpr arrs objE
                validEval sol = case evalExprVal consts arrs sol expandedObj of
                    EvalOk val -> Just (val, sol)
                    _          -> Nothing
                evaluated = [p | Just p <- map validEval solutions]
            in if null evaluated
                   then Nothing
                   else Just (snd (minimumBy (comparing fst) evaluated))
        Maximize objE ->
            let expandedObj   = expandExpr arrs objE
                validEval sol = case evalExprVal consts arrs sol expandedObj of
                    EvalOk val -> Just (val, sol)
                    _          -> Nothing
                evaluated = [p | Just p <- map validEval solutions]
            in if null evaluated
                   then Nothing
                   else Just (snd (maximumBy (comparing fst) evaluated))
  where
    -- (AI-Assists) Part D: Flatten array declarations into individual elements (e.g. q[1])
    processDeclarations :: [Declaration]
                        -> ( [(Identifier, Int)]
                           , [(Identifier, Int)]
                           , [(Identifier, [Int])]
                           )
    processDeclarations = foldl step ([], [], [])
      where
        step (cs, as, vs) (ConstantDeclaration name e) =
            case evalExprVal cs as [] e of
                EvalOk val -> (cs ++ [(name, val)], as, vs)
                _          -> (cs, as, vs)
        step (cs, as, vs) (VariableDeclaration name dom) =
            (cs, as, vs ++ [(name, domainValues dom)])
        step (cs, as, vs) (ArrayDeclaration name lenE dom) =
            let len = case evalExprVal cs as [] lenE of
                    EvalOk n -> n
                    _        -> 0
                elems = [ (name ++ "[" ++ show i ++ "]", domainValues dom)
                        | i <- [1..len]
                        ]
            in (cs, as ++ [(name, len)], vs ++ elems)

-- | ISO 8601 current timestamp formatted as YYYY-MM-DDTHH-MM-SS.
getTime :: IO String
getTime = formatTime defaultTimeLocale "%Y-%m-%dT%H-%M-%S" <$> getCurrentTime