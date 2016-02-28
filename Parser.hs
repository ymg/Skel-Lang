module Parser where

import           Text.Parsec
import           Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr   as Ex
import qualified Text.Parsec.Token  as Tok

import           Lexer
import           Syntax

binary s f assoc = Ex.Infix (reservedOp s >> return (BinOp f)) assoc

table = [[binary "*" Times Ex.AssocLeft,
          binary "/" Divide Ex.AssocLeft]
        ,[binary "+" Plus Ex.AssocLeft,
          binary "-" Minus Ex.AssocLeft]]

pars :: Parser Pars
pars = many $ do
     parList <- parT
     return parList

parT :: Parser Par
parT =  try parFun
    <|> try parParallel
    <|> try parFarm

parFarm :: Parser Par
parFarm = do
  reserved "farm"
  num <- integer
  par <- pars
  return $ Farm num par

parFun :: Parser Par
parFun = do
  reserved "func"
  name <- identifier
  struc <- many struct
  return $ Function name struc

parParallel :: Parser Par
parParallel = do
  reserved "func"
  name <- identifier
  struc <- many struct
  return $ Function name struc

int :: Parser Expr
int = do
  n <- integer
  return $ Digit (fromIntegral n)

floating :: Parser Expr
floating = do
  n <- float
  return $ Float n

expr :: Parser Expr
expr = Ex.buildExpressionParser table factor

structExpr :: Parser Struct
structExpr = do
  e <- factor
  return $ ExprList e

struct :: Parser Struct
struct = try structExpr
     <|> try struct
     <|> try iter

structs :: Parser [Struct]
structs = many $ do
  struc <- struct
  reservedOp ";"
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
      <|> variable
      <|> parens expr

variable :: Parser Expr
variable = do
  var <- identifier
  return $ Var var

program :: Parser Program
program = do
  reserved "program"
  name <- identifier
  reservedOp ":"
  parStmts <- pars
  return $ Program name parStmts

defn :: Parser Program
defn = try program

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

toplevel :: Parser [Program]
toplevel = many $ do
    def <- defn
    reservedOp "."
    return def


parseExpr :: String -> Either ParseError Expr
parseExpr s = parse (contents expr) "<stdin>" s

parseToplevel :: String -> Either ParseError [Program]
parseToplevel s = parse (contents toplevel) "<stdin>" s
