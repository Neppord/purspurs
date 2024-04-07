module PursPurs.Expression where

import Prelude

import Data.FunctorWithIndex (mapWithIndex)
import Data.Map.Internal (Map)
import Data.Tuple (Tuple(Tuple))
import Data.Array (intercalate) as Array
import Data.List (intercalate) as List
import Data.Map.Internal (values) as Map


data Binder
    = BinderValue Value
    | BinderVariable String
    | BinderWildcard
    | BinderError

instance Show Binder where
    show (BinderValue v) = show v
    show (BinderVariable v) = v
    show (BinderWildcard) = "_"
    show BinderError = show "<Binder Error>"

instance Eq Binder where
    eq (BinderValue x) (BinderValue x_) = x == x_
    eq (BinderVariable x) (BinderVariable x_) = x == x_
    eq (BinderWildcard) (BinderWildcard) = true
    eq _ _ = false

type Env = Map String Value

data Expr
  = ExprError
  | ExprIdentifier String
  | ExprValue Value
  | ExprArray (Array Expr)
  | ExprApp Expr Expr
  | ExprLet (Map String Expr) Expr
  | ExprIfElse Expr Expr Expr
  | ExprCase Expr (Array (Tuple Binder Expr))
  | ExprConstructor String (Array Expr)
  | ExprLambda String Expr

instance Show Expr where
  show ExprError = "<Expr Error>"
  show (ExprValue value) = show value
  show (ExprApp f x) = "(" <> show f <> " " <> show x <> ")"
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
  eq (ExprIfElse i t e) (ExprIfElse i_ t_ e_) = i == i_ && t == t_ && e == e_
  eq (ExprCase m b) (ExprCase m_ b_) = m == m_ && b == b_
  eq (ExprLet f x) (ExprLet g y) = x == y && f == g
  eq (ExprArray x) (ExprArray y) = x == y
  eq (ExprConstructor name array) (ExprConstructor name_ array_) = name == name_ && array == array_
  eq (ExprLambda parameter expression) (ExprLambda parameter_ expression_) =
    parameter == parameter_
      && expression == expression_
  eq _ _ = false

data Value
  = ValueVoid
  | ValueError
  | ValueBoolean Boolean
  | ValueInt Int
  | ValueChar Char
  | ValueNumber Number
  | ValueString String
  | ValueArray (Array Value)
  | ValueLambda String Env Expr
  | ValueConstructor String (Array Value)
  | ValueForeignFn (Value -> Value)

instance Show Value where
  show ValueVoid = "Void"
  show ValueError = "<Value Error>"
  show (ValueBoolean b) = show b
  show (ValueChar b) = show b
  show (ValueString s) = show s
  show (ValueNumber s) = show s
  show (ValueArray a) = show a
  show (ValueInt i) = show i
  show (ValueForeignFn _) = "<foreign>"
  show (ValueLambda param _ expr) = "(\\" <> param <> " -> " <> show expr <> ")"
  show (ValueConstructor name values) =
    if values == [] then name
    else
      "(" <> name <> " " <> (values <#> show # Array.intercalate " ") <> ")"

instance Eq Value where
  eq ValueVoid ValueVoid = true
  eq (ValueBoolean x) (ValueBoolean y) = x == y
  eq (ValueChar x) (ValueChar y) = x == y
  eq (ValueNumber x) (ValueNumber y) = x == y
  eq (ValueInt x) (ValueInt y) = x == y
  eq (ValueString x) (ValueString y) = x == y
  eq (ValueArray x) (ValueArray y) = x == y
  eq (ValueLambda param env expr) (ValueLambda param_ env_ expr_) =
    param == param_
      && env == env_
      && expr == expr_
  eq (ValueConstructor name values) (ValueConstructor name_ values_) = name == name_ && values == values_
  eq _ _ = false
