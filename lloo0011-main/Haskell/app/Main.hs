{-# LANGUAGE OverloadedStrings #-}

-- | Main module for launching the web server that handles parsing and
-- solving MiniZinc-lite models. This server exposes an API that accepts a
-- model, parses and solves it, and returns the resulting assignment.
module Main (main) where

import Assignment (miniZincParser, solveProblem)
import Control.Arrow (second)
import Control.Concurrent.QSemN (newQSemN)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, (.=))
import Data.Aeson.Key (fromString)
import Data.Text.Lazy (Text, pack, unpack)
import Instances (ParseResult (..), parse)
import Web.Scotty (ActionM, formParam, json, post, scotty)

-- Magic code to convert key, value pairs to JSON to send back to the server
jsonResponse :: [(String, String)] -> ActionM ()
jsonResponse pairs =
    json $
        object
            [ fromString key .= (pack value :: Text) | (key, value) <- pairs
            ]

-- --------------------------------------------------------------------------------
-- | Entry point – launches Scotty web server and defines API routes
main :: IO ()
main =
    newQSemN 1 >>= \_ -> scotty 3000 $ do
        post "/api/minizinc" $ do
            modelText <- formParam "model" :: ActionM Text

            liftIO $ putStrLn (unpack modelText)

            case parse miniZincParser (unpack modelText) of
                Error err ->
                    jsonResponse [("error", show err)]
                Result _ model ->
                    case solveProblem model of
                        Nothing ->
                            jsonResponse [("result", "No solution")]
                        Just env ->
                            jsonResponse (map (second show) env)
