module Main where

import Syntax
import Parser
import Interpreter

import System.Environment (getArgs)
import Control.Monad (liftM)

main :: IO ()
main = do
  file <- liftM (!! 0) getArgs
  contents <- readFile file
  let prog = parseProgram contents
  --interpret prog
  return ()

