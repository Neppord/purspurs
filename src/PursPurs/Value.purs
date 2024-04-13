module PursPurs.Value where

import Prelude

import Data.Map.Internal (Map)
import Data.Array (fromFoldable, intercalate, null) as Array
import Data.Map.Internal (empty, insert, lookup, union) as Map
import Data.Maybe (fromMaybe) as Maybe
import Data.Map (keys) as Map
import PursPurs.Operator (Operator)



type Values expr = Map String (Value expr)
type Operators expr = Map String (Operator expr)
type Env expr =
  { values :: Values expr
  , operators :: Operators expr
  }

empty_env :: forall expr. Env expr
empty_env = { values: Map.empty, operators: Map.empty}

insert_all :: forall expr. Values expr -> Env expr -> Env expr
insert_all values env = env { values = Map.union env.values values }

cant_find :: forall expr. String -> Value expr
cant_find key = ValueError $ "Could not find " <> key <> " in scope"

lookup :: forall expr. String -> Env expr -> Value expr
lookup key env = env.values
  # Map.lookup key
  # Maybe.fromMaybe (cant_find key)

insert :: forall expr. String -> Value expr -> Env expr -> Env expr
insert key value env = env { values = Map.insert key value env.values }

names :: forall expr. Env expr -> Array String
names env = Map.keys env.values # Array.fromFoldable

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
