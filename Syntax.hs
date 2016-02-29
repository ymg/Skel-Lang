module Syntax where

type Name = String
type Structs = [Struct]
type Pars = [Par]
type Exprs = [Expr]

data Program
  = Program Name Pars
  deriving (Eq, Ord, Show)

data Expr
  = Digit Integer
  | Float Double
  | Logic Bool
  | Var String
  | Exception
  | BinOp Op Expr Expr
  deriving (Eq, Ord, Show)

data Op
  = Plus
  | Minus
  | Times
  | Divide
  deriving (Eq, Ord, Show)

data Par
  = Function Name Structs
  | Farm Integer Par
  deriving (Eq, Ord, Show)

data Struct
  = ExprList Expr
  | CompOp Structs
  | Iter Integer Structs
  deriving (Eq, Ord, Show)





