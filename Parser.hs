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
     reservedOp ";"
     return _par

parT :: Parser Par
parT =  try parFun
    <|> try parFarm

parFarm :: Parser Par
parFarm = do
  reserved "farm"
  num <- integer
  par <- parT
  return $ Farm num par

parFun :: Parser Par
parFun = do
  reserved "func"
  name <- identifier
  struc <- structs
  return $ Function name struc

parParallel :: Parser Pars
parParallel = many $ do
  p <- parT
  reservedOp "||"
  return p

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

structExpr :: Parser Struct
structExpr = do
  ex <- factor
  return $ ExprList ex

struct :: Parser Struct
struct = try structExpr
     <|> try struct
     <|> try iter

structs :: Parser Structs
structs = do
  struc <- many struct
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
  _pars <- many pars
  return $ Program name _pars

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


parsePar :: String -> Either ParseError Par
parsePar s = parse (contents parT) "<stdin>" s

parseStruct :: String -> Either ParseError Struct
parseStruct s = parse (contents struct) "<stdin>" s

parseExpr :: String -> Either ParseError Expr
parseExpr s = parse (contents expr) "<stdin>" s

parseToplevel :: String -> Either ParseError [Program]
parseToplevel s = parse (contents toplevel) "<stdin>" s
