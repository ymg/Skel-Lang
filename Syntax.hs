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
  | Bool Bool
  | String String
  | BinOp Op Expr Expr
  | Var String
  | Assign Exprs
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
  = ExprList Exprs
  | CompOp Struct
  | Iter Integer Structs
  deriving (Eq, Ord, Show)





