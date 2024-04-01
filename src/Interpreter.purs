module Interpreter where

import Prelude

import Data.Map.Internal (Map)
import Data.Maybe (Maybe(..))
import Parser (Expr(..), Value(..))
import Data.Map.Internal (lookup) as Map

type Env = Map String Value

interpret_expr :: Env -> Expr -> Value
interpret_expr _ (ExprValue value) = value
interpret_expr env (ExprIdentifier key) = case Map.lookup key env of
  Just v -> v
  Nothing -> ValueError
interpret_expr env (ExprArray values) = ValueArray (values <#> interpret_expr env)
interpret_expr _ _ = ValueError