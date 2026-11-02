module Assignment
    ( ADT (..)
    , Declaration (..)
    , Domain (..)
    , Expr (..)
    , Constraint (..)
    , CompareOperator (..)
    , SolverGoal (..)
    , miniZincParser
    , solveProblem
    , getTime
    ) where

import Control.Applicative ((<|>))
import Data.Maybe (listToMaybe)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)

import Instances (ParseError (UnexpectedString), Parser)
import Parser (failed)

--------------------------------------------------------------------------------
-- AST
--------------------------------------------------------------------------------

type Identifier = String

data ADT = ADT
    { declarations :: [Declaration]
    , constraints :: [Constraint]
    , solverGoal :: SolverGoal
    }
    deriving (Eq, Show)

data Declaration
    = ConstantDeclaration Identifier Expr
    | VariableDeclaration Identifier Domain
    deriving (Eq, Show)

data Domain = Range Int Int
    | FiniteSet [Int]
    deriving (Eq, Show)

data Expr
    = IntLit Int
    | Var Identifier
    | Neg Expr
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Pow Expr Expr
    | MacroExpr MacroName [Expr]
    deriving (Eq, Show)

data CompareOperator = OpEq | OpNeq | OpLt | OpLte | OpGt | OpGte
    deriving (Eq, Show)

data Constraint = Compare CompareOperator Expr Expr
    | All [Constraint]
    | Any [Constraint]
    | AllDifferent [Expr]
    deriving (Eq, Show)

data MacroName = Sum | Min | Max
    deriving (Eq, Show)

data SolverGoal = Satisfy
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
whiteSpace = many (satisfy (`elem` " \t\r\n\f\v") *> pure () <|> comment) *> pure ()

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
    Add a b          -> (+) <$> evalConstExpr consts a <*> evalConstExpr consts b
    Sub a b          -> (-) <$> evalConstExpr consts a <*> evalConstExpr consts b
    Mul a b          -> (*) <$> evalConstExpr consts a <*> evalConstExpr consts b
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
        <|> pVar env
        <|> (IntLit <$> pIntLiteral)

-- Parse a variable and verify it has already been declared
pVar :: ParseEnv -> Parser Expr
pVar env = do
    name <- rawIdentifier
    let isConst = name `elem` map fst (envConstants env)
        isVar   = name `elem` envVars env
    if isConst || isVar
        then pure (Var name)
        else failed (UnexpectedString ("Undefined variable: " ++ name))

--  Parse numeric macros: sum, min, max
pNumericMacroExpr :: ParseEnv -> Parser Expr
pNumericMacroExpr env = do
    macro <- (keyword "sum" *> pure Sum)
         <|> (keyword "min" *> pure Min)
         <|> (keyword "max" *> pure Max)
    _  <- tokenChar '(' *> tokenChar '['
    es <- sepBy (pExpr env) (tokenChar ',')
    _  <- tokenChar ']' <* tokenChar ')'
    pure (MacroExpr macro es)

-- Parse a single comparison: <expr> <op> <expr>
pCompItem :: ParseEnv -> Parser Constraint
pCompItem env = do
    e1 <- pExpr env
    op <- pCompareOp
    e2 <- pExpr env
    pure (Compare op e1 e2)


--------------------------------------------------------------------------------
-- Part A: Declarations, Constraints & Model Entry Point
--------------------------------------------------------------------------------

-- (AI assists) Parse constraint statements including boolean macros.
pConstraint :: ParseEnv -> Parser Constraint
pConstraint env = keyword "constraint" *> pBody <* tokenChar ';'
  where
    pBody = (keyword "allDifferent" *> pAllDiff)
        <|> (keyword "all"          *> pAll)
        <|> (keyword "any"          *> pAny)
        <|> pCompItem env

    pAllDiff = do
        _  <- tokenChar '(' *> tokenChar '['
        es <- sepBy (pExpr env) (tokenChar ',')
        _  <- tokenChar ']' <* tokenChar ')'
        pure (AllDifferent es)

    pAll = do
        _  <- tokenChar '(' *> tokenChar '['
        cs <- sepBy (pCompItem env) (tokenChar ',')
        _  <- tokenChar ']' <* tokenChar ')'
        pure (All cs)

    pAny = do
        _  <- tokenChar '(' *> tokenChar '['
        cs <- sepBy (pCompItem env) (tokenChar ',')
        _  <- tokenChar ']' <* tokenChar ')'
        pure (Any cs)

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
    if name `elem` (map fst (envConstants env) ++ envVars env)
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

data Statement
    = DeclStmt Declaration ParseEnv
    | ConstrStmt Constraint

pStatement :: ParseEnv -> Parser Statement
pStatement env = (uncurry DeclStmt <$> pConstantDecl env)
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
-- Part B: Incremental Constraint Solver & Optimizer
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

--  Evaluate an expression under partial assignments.
evalExprVal :: [(Identifier, Int)]
            -> [Binding]
            -> Expr
            -> EvalResult Int
evalExprVal consts solved expr = case expr of
    IntLit n -> EvalOk n
    Var x    ->
        case lookup x consts of
            Just v  -> EvalOk v
            Nothing -> case lookup x solved of
                Just v  -> EvalOk v
                Nothing -> Unassigned
    Neg e    -> negate <$> evalExprVal consts solved e
    Add a b  -> (+) <$> evalExprVal consts solved a
                    <*> evalExprVal consts solved b
    Sub a b  -> (-) <$> evalExprVal consts solved a
                    <*> evalExprVal consts solved b
    Mul a b  -> (*) <$> evalExprVal consts solved a
                    <*> evalExprVal consts solved b
    Div a b  -> do
        va <- evalExprVal consts solved a
        vb <- evalExprVal consts solved b

        if vb == 0 then EvalError else EvalOk (va `div` vb)
    Pow a b  -> do
        va <- evalExprVal consts solved a
        vb <- evalExprVal consts solved b
        if vb < 0 then EvalError else EvalOk (va ^ vb)
    MacroExpr m es -> do
        vs <- traverse (evalExprVal consts solved) es
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
                          -> [Binding]
                          -> Constraint
                          -> EvalResult Bool
evalConstraintIncremental consts solved constraint =
    case constraint of
        Compare op a b ->
            applyCompare op <$> evalExprVal consts solved a
                            <*> evalExprVal consts solved b
        AllDifferent es ->
            let results = map (evalExprVal consts solved) es
            in if any (== EvalError) results
                   then EvalOk False
                   else let okVals = [v | EvalOk v <- results]
                        in if length okVals /= length (nub okVals)
                               -- Duplicate detected among assigned variables
                               then EvalOk False
                               else if any (== Unassigned) results
                                        then Unassigned
                                        else EvalOk True
        All cs ->
            let results = map (evalConstraintIncremental consts solved) cs
            in if any (== EvalOk False) results || any (== EvalError) results
                   then EvalOk False
                   else if any (== Unassigned) results
                            then Unassigned
                            else EvalOk True
        Any cs ->
            let results = map (evalConstraintIncremental consts solved) cs
            in if any (== EvalOk True) results
                   then EvalOk True
                   else if all (== EvalOk False) results
                            then EvalOk False
                            else Unassigned

-- | (AI-Assisted) Backtracking search engine with early failure detection.
searchWithPruning :: [(Identifier, Int)]
                  -> [(Identifier, [Int])]
                  -> [Constraint]
                  -> [Binding]
                  -> [[Binding]]
searchWithPruning consts remainingVars constrs currentSolved =
    case remainingVars of
        [] -> [reverse currentSolved]
        (v, dom) : rest -> do
            val <- dom
            let nextSolved = (v, val) : currentSolved
                evals = map
                    (evalConstraintIncremental consts nextSolved)
                    constrs
            -- Prune search immediately if an evaluable constraint failed
            if any (== EvalOk False) evals
                then []
                else searchWithPruning consts rest constrs nextSolved

-- | (AI-Assisted) Solve the problem and satisfy or optimize the goal.
solveProblem :: ADT -> Maybe [(String, Int)]
solveProblem adt =
    let (consts, decVars) = processDeclarations (declarations adt)
        solutions         = searchWithPruning
            consts
            decVars
            (constraints adt)
            []
    in case solverGoal adt of
        Satisfy -> listToMaybe solutions
        Minimize objE ->
            let validEval sol = case evalExprVal consts sol objE of
                    EvalOk val -> Just (val, sol)
                    _          -> Nothing
                evaluated = [p | Just p <- map validEval solutions]
            in if null evaluated
                   then Nothing
                   else Just (snd (minimumBy (comparing fst) evaluated))
        Maximize objE ->
            let validEval sol = case evalExprVal consts sol objE of
                    EvalOk val -> Just (val, sol)
                    _          -> Nothing
                evaluated = [p | Just p <- map validEval solutions]
            in if null evaluated
                   then Nothing
                   else Just (snd (maximumBy (comparing fst) evaluated))
  where
    processDeclarations :: [Declaration]
                        -> ([(Identifier, Int)], [(Identifier, [Int])])
    processDeclarations = foldl step ([], [])
      where
        step (cs, vs) (ConstantDeclaration name e) =
            case evalConstExpr cs e of
                Just val -> ((name, val) : cs, vs)
                Nothing  -> (cs, vs)
        step (cs, vs) (VariableDeclaration name dom) =
            (cs, vs ++ [(name, domainValues dom)])

-- | ISO 8601 current timestamp formatted as YYYY-MM-DDTHH-MM-SS.
getTime :: IO String
getTime = formatTime defaultTimeLocale "%Y-%m-%dT%H-%M-%S" <$> getCurrentTime