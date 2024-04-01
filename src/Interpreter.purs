module Interpreter where

import Prelude

import Data.Map.Internal (Map)
import Data.Maybe (Maybe(..))
import Parser (Expr(..), Value(..))
import Data.Map.Internal (lookup) as Map

type Env = Map String Value

evaluate_expr :: Env -> Expr -> Value
evaluate_expr _ (ExprValue value) = value
evaluate_expr env (ExprIdentifier key) = case Map.lookup key env of
  Just v -> v
  Nothing -> ValueError
evaluate_expr env (ExprArray values) = ValueArray (values <#> evaluate_expr env)
evaluate_expr _ _ = ValueError