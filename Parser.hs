module Parser where

import           Debug.Trace
import           Lexer
import           Syntax

import           Text.Parsec
import qualified Text.Parsec.Expr   as Ex
import           Text.Parsec.String (Parser)
import qualified Text.Parsec.Token  as Tok


binary s f assoc = Ex.Infix (reservedOp s >> return (BinOp f)) assoc

table = [[binary "*" Times Ex.AssocLeft,
          binary "/" Divide Ex.AssocLeft]
        ,[binary "+" Plus Ex.AssocLeft,
          binary "-" Minus Ex.AssocLeft]]

pars :: Parser Par
pars = do
     _par <- parT
     return $ _par

parT :: Parser Par
parT =  try parFun
    <|> try parFarm
    <|> try parParallel

parFarm :: Parser Par
parFarm = do
  reserved "farm"
  num <- integer
  par <- parens $ parT
  return $ Farm num par

parFun :: Parser Par
parFun = do
  reserved "func"
  name <- identifier
  struc <- try (parens $ structs) <|> try structs
  return $ Function name struc

parParallel :: Parser Par
parParallel = do
  reservedOp "||"
  p <- parT
  return $ Par p

int :: Parser Expr
int = do
  n <- integer
  return $ Digit (fromInteger n)

floating :: Parser Expr
floating = do
  n <- float
  return $ Float n

expr :: Parser Expr
expr = Ex.buildExpressionParser table factor

exprs :: Parser Exprs
exprs = many1 $ do
  exprs_ <- try (do
    reservedOp ","
    y <- expr
    return y) <|> 
    try (do
      i <- expr
      return i)
  return exprs_

{--exprs :: Parser Exprs
exprs = many1 $ do
  reservedOp ","
  exprs_ <- factor
  return exprs_--}

structExprs :: Parser Struct
structExprs = do
  ex <- exprs -- <|> try (many1 factor)
  return $ ExprList ex

struct :: Parser Struct
struct = try structExprs
      <|> try structCompOp
      <|> try iter

structCompOp :: Parser Struct
structCompOp = do
    reservedOp "•"
    _struct <- struct
    return $ CompOp _struct

structs :: Parser Structs
structs = many $ do
       struc <- struct
       return struc

iter :: Parser Struct
iter = do
  reserved "iter"
  i <- integer
  structs <- many struct
  return $ Iter i structs

factor :: Parser Expr
factor = try floating
      <|> try int
      <|> try bool
      <|> try str
      <|> try assignmentExpr
      <|> variable
      <|> parens expr

str :: Parser Expr
str = do
  str <- Lexer.string
  return $ String str

bool :: Parser Expr
bool = (Bool True <$ reserved "True")
    <|> (Bool False <$ reserved "False")

variable :: Parser Expr
variable = do
  var <- identifier
  return $ Var var

{-variable :: Parser Expr
variable = do
  var <- identifier
  reservedOp "="
  assign <- try (many1 expr)
  return $ Var var assign-}

assignmentExpr :: Parser Expr
assignmentExpr = do
  reservedOp "="
  val <- many $ do expr
  return $ Assign val

program :: Parser Program
program = do
  reserved "program"
  name <- identifier
  reservedOp ":"
  _pars <- many pars
  reservedOp "."
  return $ Program name _pars

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

toplevel :: Parser Program
toplevel = do
    def <- try program
    return def


parsePar :: String -> Either ParseError Par
parsePar s = parse (contents parT) "<stdin>" s

parseStruct :: String -> Either ParseError Struct
parseStruct s = parse (contents struct) "<stdin>" s

parseExpr :: String -> Either ParseError Expr
parseExpr s = parse (contents expr) "<stdin>" s

parseToplevel :: String -> Either ParseError Program
parseToplevel s = parse (contents toplevel) "<stdin>" s
