module Main where

import Syntax
import Parser
import Checker
import Interpreter

import System.Environment (getArgs)
import Control.Monad (liftM)

main :: IO ()
main = do
  file <- liftM (!! 0) getArgs
  contents <- readFile file
  let prog = parseLL contents
  case (typecheck prog) of
    Left e -> putStrLn $ show e
    Right _ ->
      putStrLn $ show $ interpretProgram prog

