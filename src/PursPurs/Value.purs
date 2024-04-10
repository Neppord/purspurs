module PursPurs.Value where

import Prelude
import Data.Map.Internal (Map)
import Data.Array (intercalate, null) as Array


type Env expr = Map String (Value expr)

data Value expr
  = ValueVoid
  | ValueError String
  | ValueBoolean Boolean
  | ValueInt Int
  | ValueChar Char
  | ValueNumber Number
  | ValueString String
  | ValueArray (Array (Value expr))
  | ValueLambda String (Env expr) expr
  | ValueConstructor String (Array (Value expr))
  | ValueForeignFn (Value expr -> Value expr)

instance Show expr => Show (Value expr) where
  show ValueVoid = "Void"
  show (ValueError msg) = "<Value Error: " <> msg <> ">"
  show (ValueBoolean b) = show b
  show (ValueChar b) = show b
  show (ValueString s) = show s
  show (ValueNumber s) = show s
  show (ValueArray a) = show a
  show (ValueInt i) = show i
  show (ValueForeignFn _) = "<foreign>"
  show (ValueLambda param _ expr) = "(\\" <> param <> " -> " <> show expr <> ")"
  show (ValueConstructor name values) =
    if Array.null values then name
    else
      "(" <> name <> " " <> (values <#> show # Array.intercalate " ") <> ")"

instance Eq expr => Eq (Value expr) where
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
