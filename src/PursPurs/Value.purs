module PursPurs.Value where

import Prelude

import Data.Map.Internal (Map)
import Data.Array (fromFoldable, intercalate, null) as Array
import Data.Map.Internal (empty, insert, lookup, union) as Map
import Data.Maybe (fromMaybe) as Maybe
import Data.Map (keys) as Map
import PursPurs.Operator (Operator)
import Data.Maybe (Maybe)

type Values expr = Map String (Value expr)
type Operators value = Map String (Operator value)
type Scope expr =
  { values :: Values expr
  , operators :: Operators (Callable expr)
  }

empty_scope :: forall expr. Scope expr
empty_scope = { values: Map.empty, operators: Map.empty }

insert_all :: forall expr. Values expr -> Scope expr -> Scope expr
insert_all values env = env { values = Map.union env.values values }

cant_find :: forall expr. String -> Value expr
cant_find key = ValueError $ "Could not find " <> key <> " in scope"

lookup :: forall expr. String -> Scope expr -> Value expr
lookup key env = env.values
  # Map.lookup key
  # Maybe.fromMaybe (cant_find key)

lookup_callable :: forall expr. String -> Scope expr -> Callable expr
lookup_callable key env = lookup key env # case _ of
  ValueCallable c -> c
  ValueError msg -> CallableError msg
  _ -> CallableError (key <> " is not callable")

lookup_operator :: forall expr. String -> Scope expr -> Maybe (Operator (Callable expr))
lookup_operator key env = env.operators
  # Map.lookup key

insert :: forall expr. String -> Value expr -> Scope expr -> Scope expr
insert key value env = env { values = Map.insert key value env.values }

insert_operator :: forall expr. String -> Operator (Callable expr) -> Scope expr -> Scope expr
insert_operator key operator env = env { operators = Map.insert key operator env.operators }

names :: forall expr. Scope expr -> Array String
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
  | ValueConstructor String (Array (Value expr))
  | ValueCallable (Callable expr)

data Callable expr
  = CallableLambda String (Scope expr) expr
  | CallableForeignFn (Value expr -> Value expr)
  | CallableError String

instance Show expr => Show (Callable expr) where
  show (CallableForeignFn _) = "<foreign>"
  show (CallableLambda param _ expr) = "(\\" <> param <> " -> " <> show expr <> ")"
  show (CallableError msg) = "<Call Error: " <> msg <> ">"

instance Eq expr => Eq (Callable expr) where
  eq (CallableLambda param env expr) (CallableLambda param_ env_ expr_) =
    param == param_ && env == env_ && expr == expr_
  eq _ _ = false

instance Show expr => Show (Value expr) where
  show ValueVoid = "Void"
  show (ValueError msg) = "<Value Error: " <> msg <> ">"
  show (ValueBoolean b) = show b
  show (ValueChar b) = show b
  show (ValueString s) = show s
  show (ValueNumber s) = show s
  show (ValueArray a) = show a
  show (ValueInt i) = show i
  show (ValueCallable f) = show f
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
  eq (ValueCallable f) (ValueCallable f_) = f == f_
  eq (ValueConstructor name values) (ValueConstructor name_ values_) = name == name_ && values == values_
  eq _ _ = false
