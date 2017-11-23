module Main where

import Syntax
import Machine
import Parser
import Data.Map.Lazy as M
import System.Environment (getArgs)
import Control.Monad (liftM)

main :: IO ()
main = do
  file <- liftM (!! 0) getArgs
  contents <- readFile file
  let prog = parseProgram contents
  let initState = initialState prog
  let finalState = interpret initState
  showState finalState

testProgram1 :: IO ()
testProgram1 = do
  let (heap, regs, inst) = prog1InitState
  let finalState = interpret (heap, regs, Jump (OpLabel $ Label "prod"))
  showState finalState

prog1InitState :: MachineState
prog1InitState =
  let heap = M.map HeapInst (M.fromList
                             [ (Label "prod", prod)
                             , (Label "loop", loop)
                             , (Label "done", done)
                             , (Label "exit", halt) ])
      regs = M.fromList [ (Register 1, OpLit 2)
                        , (Register 2, OpLit 2)
                        , (Register 3, OpLit 0)
                        , (Register 4, OpLabel $ Label "exit") ]
      inst = Jump (OpLabel $ Label "prod")
  in (heap, regs, inst)
  where prod = toInstSeq [ Move (Register 3) (OpLit 0) ] (Jump (OpLabel (Label "loop")))
        loop = toInstSeq [ IfZero (Register 1) (OpLabel $ Label "done")
                                    , Add (Register 3) (Register 2) (OpRegister $ Register 3)
                                    , Add (Register 1) (Register 1) (OpLit (-1)) ]
                                                                      (Jump (OpLabel (Label "loop")))
        done = Jump (OpRegister $ Register 4)
        halt = Halt
