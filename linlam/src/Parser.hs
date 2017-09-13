module Parser (parseLL) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language (emptyDef)
import Text.ParserCombinators.Parsec.Token as P
import Syntax

defLL = emptyDef
  { P.commentStart = "{-"
  , P.commentEnd = "-}"
  , P.commentLine = "--"
  , P.identStart = lower <|> char '_'
  , P.identLetter = alphaNum <|> oneOf "_'"
  , P.opStart = oneOf "\\.:"
  , P.opLetter = oneOf "\\.:"
  , P.reservedOpNames = ["\\", ".", ":"]
  , P.reservedNames = ["if", "then", "else", "split", "as", "in", "True", "False", "apply", "to"]
  }

lexerLL = makeTokenParser defLL
parensLL = P.parens lexerLL
bracketsLL = P.brackets lexerLL
anglesLL = P.angles lexerLL
identifierLL = P.identifier lexerLL
reservedLL = P.reserved lexerLL
commaLL = P.comma lexerLL
colonLL = P.colon lexerLL
dotLL = P.dot lexerLL
symbolLL = P.symbol lexerLL
opLL = P.operator lexerLL
whitespaceLL = P.whiteSpace lexerLL
bracesLL = P.braces lexerLL

parseLL :: String -> Term
parseLL prog =
  case (parse parseProgram "program" prog) of
    Left err -> error $ "unable to parse: " ++ show err
    Right t -> t

parseProgram :: Parser Term
parseProgram = do
  whitespaceLL
  term <- parseTerm
  eof
  return term

parseTerm :: Parser Term
parseTerm
  =   parensLL parseTerm
  <|> fmap TVar parseVar
  <|> parseBool
  <|> parseIf
  <|> parsePair
  <|> parseSplit
  <|> parseAbs
  <|> parseApp

parseVar :: Parser Var
parseVar = do
  v <- identifierLL
  return $ Var $ v

parseBool :: Parser Term
parseBool
  =   do reservedLL "True"
         q <- bracketsLL parseQualifier
         return $ TBool q BTrue
  <|> do string "False"
         q <- bracketsLL parseQualifier
         return $ TBool q BFalse

parseIf :: Parser Term
parseIf = do
  reservedLL "if"
  t1 <- parseTerm
  reservedLL "then"
  t2 <- parseTerm
  reservedLL "else"
  t3 <- parseTerm
  return $ TIf t1 t2 t3

parsePair :: Parser Term
parsePair = do
  (t1, t2) <- anglesLL $
    do t1 <- parseTerm
       commaLL
       t2 <- parseTerm
       return (t1, t2)
  q <- bracketsLL parseQualifier
  return $ TPair q t1 t2

parseSplit :: Parser Term
parseSplit = do
  reservedLL "split"
  t1 <- parseTerm
  reservedLL "as"
  x <- parseVar
  commaLL
  y <- parseVar
  reservedLL "in"
  t2 <- parseTerm
  return $ TSplit t1 x y t2

parseAbs :: Parser Term
parseAbs = do
  symbolLL "\\"
  q <- bracketsLL parseQualifier
  x <- parseVar
  colonLL
  typ <- parseType
  dotLL
  t2 <- parseTerm
  return $ TAbs q x typ t2

-- TODO: a hack, tired of dealing with parsing/ambiguity
parseApp :: Parser Term
parseApp = do
  reservedLL "apply"
  t1 <- parseTerm
  reservedLL "to"
  t2 <- parseTerm
  return $ TApp t1 t2

parseQualifier :: Parser Qualifier
parseQualifier
  =   do symbolLL "u"
         return Unrestricted
  <|> do symbolLL "l"
         return Linear

parseType :: Parser Type
parseType = do
  p <- parsePretype
  q <- bracketsLL parseQualifier
  return $ Type q p

parsePretype :: Parser Pretype
parsePretype
  =   parseBoolType
  <|> parsePairType
  <|> parensLL parseFuncType

parseBoolType :: Parser Pretype
parseBoolType = do
  reservedLL "Bool"
  return PBool

parsePairType :: Parser Pretype
parsePairType = do
  (typ1, typ2) <- anglesLL $
    do typ1 <- parseType
       commaLL
       typ2 <- parseType
       return (typ1, typ2)
  return $ PPair typ1 typ2

parseFuncType :: Parser Pretype
parseFuncType = do
  typ1 <- parseType
  symbolLL "->"
  typ2 <- parseType
  return $ PFunc typ1 typ2
