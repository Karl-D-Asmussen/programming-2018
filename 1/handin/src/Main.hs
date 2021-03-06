-- | This module defines a simple command line interface for the SubScript
-- interpreter.  If your solution is correct, this module should just
-- work.
module Main
       (main)
where

import SubsInterpreter

import Data.List(intercalate)
import System.Environment(getArgs)


-- | nice display of JavaScript values
nice :: Value -> String
nice (IntVal v) = show v
nice TrueVal = "true"
nice FalseVal = "false"
nice (StringVal s) = show s
nice UndefinedVal = "undefined"
nice (ArrayVal vs) = "["++ intercalate ", " (map nice vs) ++"]"

main :: IO ()
main = do args <- getArgs
          case args of
            [file] -> do
              s <- readFile file
              case runExpr (read s) of
                Left (k, e) -> error $ e ++ " (" ++ show k ++ ")"
                Right res -> putStrLn $ "Result is: " ++ nice res
            _ ->
              error "Give me a (single) argument!"
