module Interpreter where

import Prelude

import Data.Maybe (Maybe(..), isNothing, maybe)
import Data.Tuple (Tuple(Tuple), snd)
import Data.Tuple.Nested ((/\))
import Parser (parse_declaration, parse_expression)
import PursPurs.Declaration (Declaration(..))
import PursPurs.Expression (Binder(BinderConstructor, BinderError, BinderValue, BinderVariable, BinderWildcard), Expr(..))
import PursPurs.Value (Env, Value(..))
import Data.Array (any, catMaybes, findMap, foldr, fromFoldable) as Array
import Data.Map (keys) as Map
import Data.Map.Internal (empty, fromFoldable, insert, lookup, singleton, union) as Map

evaluate_expr :: Env Expr -> Expr -> Value Expr
evaluate_expr _ (ExprValue value) = value
evaluate_expr env (ExprApp f a) =
  let
    argument = evaluate_expr env a
  in
    case evaluate_expr env f of
      ValueLambda key closure expr -> evaluate_expr (closure # Map.insert key argument) expr
      ValueForeignFn fn -> fn argument
      _ -> ValueError (show f <> " is not callable")
evaluate_expr env (ExprIdentifier key) = case Map.lookup key env of
  Just v -> v
  Nothing -> ValueError ("Could not find " <> key <> " in scope")
evaluate_expr env (ExprArray values) = ValueArray (values <#> evaluate_expr env)
evaluate_expr env (ExprConstructor name values) = ValueConstructor name (values <#> evaluate_expr env)
evaluate_expr env (ExprLet m expr) =
  let
    new_env = Map.union (m <#> evaluate_expr env) env
  in
    evaluate_expr new_env expr
evaluate_expr env (ExprLambda p e) = ValueLambda p env e
evaluate_expr env (ExprIfElse i t e) = case evaluate_expr env i of
  ValueBoolean true -> evaluate_expr env t
  ValueBoolean false -> evaluate_expr env e
  _ -> ValueError "expected boolean in if"
evaluate_expr env (ExprCase expr branches) =
  let
    value = evaluate_expr env expr
    bad_value = case value of
      ValueError _ -> true
      _ -> false
    bad_branch = branches # Array.any \(Tuple binder expr_) ->
      binder == BinderError || expr_ == ExprError
    bad_case_of = bad_value || bad_branch
  in
    if bad_case_of then ValueError "Bad case of"
    else case branches # Array.findMap \(Tuple b expr_) -> match_binder value b <#> Tuple expr_ of
      Nothing -> ValueError "No matching branch in case of"
      Just (Tuple expr_ env_) -> evaluate_expr (Map.union env env_) expr_
evaluate_expr _ _ = ValueError "?"

match_binder :: Value Expr -> Binder -> Maybe (Env Expr)
match_binder value (BinderVariable name) = Just (Map.singleton name value)
match_binder _ (BinderWildcard) = Just Map.empty
match_binder value (BinderValue value_) =
  if value == value_ then Just Map.empty
  else Nothing
match_binder value (BinderConstructor name binders) = case value of
  ValueConstructor name_ values ->
    if name == name_ then
      let
        matches = match_binder <$> values <*> binders
      in
        if matches # Array.any isNothing then Nothing
        else matches
          # Array.catMaybes
          # Array.foldr Map.union Map.empty
          # Just
    else Nothing
  _ -> Nothing
match_binder _ (BinderError) = Just Map.empty

evaluate :: String -> Env Expr -> Env Expr
evaluate s env = case parse_declaration s of
  DeclarationData _ constructors -> constructors
    # Array.foldr (\(Tuple key expr) -> Map.insert key (evaluate_expr env expr)) env
    # Map.insert "_" (ValueArray (constructors <#> snd <#> evaluate_expr env))
  DeclarationValue name expr ->
    let
      value = evaluate_expr env expr
    in
      env
        # Map.insert name value
        # Map.insert "_" value
  DeclarationFixity _ _ _ _ -> env
  DeclarationError -> env # Map.insert "_" (evaluate_expr env (parse_expression s))

print :: Env Expr -> String
print env = Map.lookup "_" env # maybe "<Error>" show

names :: Env Expr -> Array String
names env = Map.keys env # Array.fromFoldable

default_env :: Env Expr
default_env = Map.fromFoldable
  [ "add" /\ ValueForeignFn case _ of
      ValueInt x -> ValueForeignFn case _ of
        ValueInt y -> ValueInt (x + y)
        _ -> ValueError "Expected Int"
      _ -> ValueError "Expected Int"
  , "mul" /\ ValueForeignFn case _ of
      ValueInt x -> ValueForeignFn case _ of
        ValueInt y -> ValueInt (x * y)
        _ -> ValueError "Expected Int"
      _ -> ValueError "Expected Int"
  ]