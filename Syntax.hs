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
  | BinOp Op Expr Expr
  | Logic Bool
  | Var String Expr
  | Exception
  deriving (Eq, Ord, Show)

data Op
  = Plus
  | Minus
  | Times
  | Divide
  deriving (Eq, Ord, Show)

data Par
  = Function Name Structs
  | Par Par
  | Farm Integer Par
  deriving (Eq, Ord, Show)

data Struct
  = ExprList Expr
  | CompOp Struct
  | Iter Integer Structs
  deriving (Eq, Ord, Show)





