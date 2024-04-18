module PursPurs.Expression where

import Prelude

import Data.FunctorWithIndex (mapWithIndex)
import Data.Map.Internal (Map)
import Data.Tuple (Tuple(Tuple))
import PursPurs.Value (Value)
import Data.Array (intercalate) as Array
import Data.List (intercalate) as List
import Data.Map.Internal (values) as Map

data Binder
  = BinderValue (Value Expr)
  | BinderVariable String
  | BinderWildcard
  | BinderConstructor String (Array Binder)
  | BinderError

instance Show Binder where
  show (BinderValue v) = show v
  show (BinderVariable v) = v
  show BinderWildcard = "_"
  show (BinderConstructor name binders) =
    "(" <> name <> (binders <#> show # Array.intercalate " ") <> ")"
  show BinderError = show "<Binder Error>"

instance Eq Binder where
  eq (BinderValue x) (BinderValue x_) = x == x_
  eq (BinderVariable x) (BinderVariable x_) = x == x_
  eq BinderWildcard BinderWildcard = true
  eq (BinderConstructor name binders) (BinderConstructor name_ binders_) =
    name == name_ && binders == binders_
  eq _ _ = false

type Branch = Tuple Binder Expr
type Branches = Array Branch

data Expr
  = ExprError
  | ExprIdentifier String
  | ExprValue (Value Expr)
  | ExprArray (Array Expr)
  | ExprApp Expr Expr
  | ExprOp Expr String Expr
  | ExprLet (Map String Expr) Expr
  | ExprIfElse Expr Expr Expr
  | ExprCase Expr Branches
  | ExprConstructor String (Array Expr)
  | ExprLambda String Expr

instance Show Expr where
  show ExprError = "<Expr Error>"
  show (ExprValue value) = show value
  show (ExprApp f x) = "(" <> show f <> " " <> show x <> ")"
  show (ExprOp l op r) = "(" <> show l <> " " <> op <> " " <> show r <> ")"
  show (ExprIfElse i t e) = "(if " <> show i <> " then " <> show t <> " else " <> show e <> ")"
  show (ExprLet m expr) = "(let\n"
    <> (m # mapWithIndex (\k v -> "  " <> k <> " = " <> show v) # Map.values # List.intercalate "\n")
    <> "\nin "
    <> (show expr)
    <> ")"
  show (ExprCase m branches) = "(case " <> show m <> " of\n"
    <> (branches <#> (\(Tuple k v) -> "  " <> show k <> " -> " <> show v) # Array.intercalate "\n")
    <> "\n)"
  show (ExprIdentifier identifier) = identifier
  show (ExprArray array) = "[" <> Array.intercalate ", " (array <#> show) <> "]"
  show (ExprConstructor name array) = ([ name ] <> (array <#> show <#> \x -> "(" <> x <> ")")) # Array.intercalate " "
  show (ExprLambda parameter expr) = "(\\" <> parameter <> " -> " <> show expr <> ")"

instance Eq Expr where
  eq (ExprIdentifier x) (ExprIdentifier y) = x == y
  eq (ExprValue x) (ExprValue y) = x == y
  eq (ExprApp f x) (ExprApp g y) = x == y && f == g
  eq (ExprOp l op r) (ExprOp l_ op_ r_) = l == l_ && op == op_ && r == r_
  eq (ExprIfElse i t e) (ExprIfElse i_ t_ e_) = i == i_ && t == t_ && e == e_
  eq (ExprCase m b) (ExprCase m_ b_) = m == m_ && b == b_
  eq (ExprLet f x) (ExprLet g y) = x == y && f == g
  eq (ExprArray x) (ExprArray y) = x == y
  eq (ExprConstructor name array) (ExprConstructor name_ array_) = name == name_ && array == array_
  eq (ExprLambda parameter expression) (ExprLambda parameter_ expression_) =
    parameter == parameter_ && expression == expression_
  eq _ _ = false
