module Interpreter where

import Prelude

import Data.Map.Internal (Map)
import Data.Maybe (Maybe(..), maybe)
import Parser (Declaration(..), Expr(..), Value(..), parse_declaration, parse_expression)
import Data.Map.Internal (empty, insert, lookup) as Map
import Data.Array (foldr) as Array
import Data.Tuple (Tuple(Tuple))

type Env = Map String Value

evaluate_expr :: Env -> Expr -> Value
evaluate_expr _ (ExprValue value) = value
evaluate_expr env (ExprApp f a) =
  let
    argument = evaluate_expr env a
  in
    case evaluate_expr env f of
      ValueLambda key expr -> evaluate_expr (env # Map.insert key argument) expr
      _ -> ValueError
evaluate_expr env (ExprIdentifier key) = case Map.lookup key env of
  Just v -> v
  Nothing -> ValueError
evaluate_expr env (ExprArray values) = ValueArray (values <#> evaluate_expr env)
evaluate_expr env (ExprConstructor name values) = ValueConstructor name (values <#> evaluate_expr env)
evaluate_expr _ _ = ValueError

interpret_expr :: String -> Value
interpret_expr expr = evaluate_expr Map.empty (parse_expression expr)

evaluate :: String -> Env -> Env
evaluate s env = case parse_declaration s of
  DeclarationData _ constructors -> constructors
    # Array.foldr (\(Tuple key value) -> Map.insert key value) env
  DeclarationValue name expr ->
    let
      value = evaluate_expr env expr
    in
      env
        # Map.insert name value
        # Map.insert "_" value
  DeclarationError -> env # Map.insert "_" (evaluate_expr env (parse_expression s))

print :: Env -> String
print env = Map.lookup "_" env # maybe "<Error>" show

default_env :: forall k v. Map k v
default_env = Map.empty