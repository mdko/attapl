module Syntax where

import Data.Map.Lazy as M

newtype Register = Register Int deriving (Ord, Eq)
newtype Label = Label String deriving (Ord, Eq)

instance Show Register where
  show (Register n) = "r" ++ show n

instance Show Label where
  show (Label l) = l

data Operand
  = OpLit Int
  | OpLabel Label
  | OpRegister Register
  deriving (Eq)

instance Show Operand where
  show (OpLit n) = show n
  show (OpLabel l) = show l
  show (OpRegister r) = show r

data Instruction
  = Move Register Operand
  | Add Register Register Operand
  | IfZero Register Operand

instance Show Instruction where
  show (Move r v) = show r ++ " := " ++ show v
  show (Add rd rs v) = show rd ++ " := " ++ show rs ++ " + " ++ show v
  show (IfZero r v) = "if " ++ show r ++ " " ++ show v

data InstSequence
  = Halt
  | Jump Operand
  | Seq Instruction InstSequence

instance Show InstSequence where
  show Halt = "\thalt"
  show (Jump v) = "\tjump " ++ show v
  show (Seq i is) = "\t" ++ show i ++ ";\n" ++ show is

type Program = M.Map Label InstSequence

toInstSeq :: [Instruction] -> InstSequence -> InstSequence
toInstSeq [] seq = seq
toInstSeq (h:tl) seq = Seq h $ toInstSeq tl seq
