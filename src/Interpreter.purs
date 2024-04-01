module Interpreter where

import Prelude

import Parser (Expr(..), Value(..))

type ExprState = {}


interpret_expr :: ExprState -> Expr -> Value
interpret_expr _ (ExprValue value) = value
interpret_expr state (ExprArray values) = ValueArray (values <#> interpret_expr state)
interpret_expr _ _ = ValueError