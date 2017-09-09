module Parser where

import Text.ParserCombinators.Parsec
import Data.Map.Lazy as M
import Syntax

-- TODO: strip comments (after // until end of line)

parseProgram :: String -> Program
parseProgram prog = do
  case (parse parseSequences "sequences" prog) of
        Left _ -> error "unable to parse"
        Right is -> M.fromList is

parseSequences :: Parser [(Label, InstSequence)]
parseSequences =
  many1 (
    do { l <- many1 letter
       ; many1 space
       ; char ':'
       ; many1 space
       ; insts <- parseInstSeq
       ; return (Label l, insts)
       })

parseInstSeq :: Parser InstSequence
parseInstSeq
  =   do { string "halt"
         ; return Halt }
  <|> do { string "jump"
         ; many1 space
         ; v <- parseOperand
         ; return (Jump v) }
  <|> do { inst <- parseInstruction
         ; many1 space
         ; char ';'
         ; many1 space
         ; is <- parseInstSeq
         ; return (Seq inst is) }

parseInstruction :: Parser Instruction
parseInstruction =
  choice
    [ do rd <- parseRegister
         many1 space
         parseAssignment
         many1 space
         rs <- parseRegister
         many1 space
         char '+'
         many1 space
         v <- parseOperand
         return (Add rd rs v)
    , do r <- parseRegister
         many1 space
         parseAssignment
         many1 space
         v <- parseOperand
         return (Move r v)
    , do string "if"
         many1 space
         r <- parseRegister
         many1 space
         v <- parseOperand
         return (IfZero r v)
    ]

parseAssignment :: Parser String
parseAssignment = string ":="

parseRegister :: Parser Register
parseRegister = do
  char 'r'
  n <- many1 digit
  return (Register $ read n)

parseOperand :: Parser Operand
parseOperand =
  choice
    [ do n <- many1 digit
         return (OpLit $ read n)
    , do r <- parseRegister
         return (OpRegister r)
    , do l <- many1 letter
         return (OpLabel $ Label l)
    ]

testParse1 :: IO ()
testParse1 = do
  case parse parseInstruction "add" "r1 := r2 + 3" of
    Right m@(Add _ _ _) -> putStrLn $ "parsed " ++ show m
    Left _ -> error "bad parse"
