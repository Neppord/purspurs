module Interpreter where

import Prelude

import Data.Maybe (Maybe(..), isNothing)
import Data.Tuple (Tuple(Tuple), snd)
import Parser (parse_declaration, parse_expression)
import PursPurs.Declaration (Declaration(..))
import PursPurs.Expression (Binder(BinderConstructor, BinderError, BinderValue, BinderVariable, BinderWildcard), Expr(..))
import PursPurs.Value (Env, Value(..), Values)
import Data.Array (any, catMaybes, findMap, foldr) as Array
import Data.Map.Internal (empty, singleton, union) as Map
import PursPurs.Value (empty_env, insert, insert_all, insert_operator, lookup, lookup_operator, names) as Value

evaluate_expr :: Env Expr -> Expr -> Value Expr
evaluate_expr _ (ExprValue value) = value
evaluate_expr env (ExprOp l op r) =
  let
    left_value = evaluate_expr env l
    right_value = evaluate_expr env r
  in
    case env # Value.lookup_operator op of
      Just { operation } -> case operation of
        ValueLambda left_key left_closure expr ->
          case evaluate_expr (left_closure # Value.insert left_key left_value) expr of
            ValueLambda right_key right_closure expr_ ->
              evaluate_expr (right_closure # Value.insert right_key right_value) expr_
            ValueForeignFn fn -> fn right_value
            _ -> ValueError (op <> " only takes one argument, but must take two")
        ValueForeignFn fn ->
          case fn left_value of
            ValueLambda right_key right_closure expr ->
              evaluate_expr (right_closure # Value.insert right_key right_value) expr
            ValueForeignFn fn_ -> fn_ right_value
            _ -> ValueError (op <> " only takes one argument, but must take two")
        _ -> ValueError ("Cant find " <> op <> " in scope")
      _ -> ValueError ("Cant find " <> op <> " in scope")

evaluate_expr env (ExprApp f a) =
  let
    argument = evaluate_expr env a
  in
    case evaluate_expr env f of
      ValueLambda key closure expr -> evaluate_expr (closure # Value.insert key argument) expr
      ValueForeignFn fn -> fn argument
      _ -> ValueError (show f <> " is not callable")
evaluate_expr env (ExprIdentifier key) = env # Value.lookup key
evaluate_expr env (ExprArray values) = ValueArray (values <#> evaluate_expr env)
evaluate_expr env (ExprConstructor name values) = ValueConstructor name (values <#> evaluate_expr env)
evaluate_expr env (ExprLet m expr) =
  let
    new_env = Value.insert_all (m <#> evaluate_expr env) env
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
      Just (Tuple expr_ values) -> evaluate_expr (Value.insert_all values env) expr_
evaluate_expr _ _ = ValueError "?"

match_binder :: Value Expr -> Binder -> Maybe (Values Expr)
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
    # Array.foldr (\(Tuple key expr) -> Value.insert key (evaluate_expr env expr)) env
    # Value.insert "_" (ValueArray (constructors <#> snd <#> evaluate_expr env))
  DeclarationValue name expr ->
    let
      value = evaluate_expr env expr
    in
      env
        # Value.insert name value
        # Value.insert "_" value
  DeclarationFixity fixity precedence function operator_name ->
    let
      operation = env # Value.lookup function
      operator =
        { operation
        , precedence
        , fixity
        }
    in
      env # Value.insert_operator operator_name operator

  DeclarationError -> env # Value.insert "_" (evaluate_expr env (parse_expression s))

print :: Env Expr -> String
print env = env
  # Value.lookup "_"
  # show

names :: Env Expr -> Array String
names = Value.names

default_env :: Env Expr
default_env = Value.empty_env
  # Value.insert "add"
      ( ValueForeignFn case _ of
          ValueInt x -> ValueForeignFn case _ of
            ValueInt y -> ValueInt (x + y)
            _ -> ValueError "Expected Int"
          _ -> ValueError "Expected Int"
      )
  # Value.insert "mul"
      ( ValueForeignFn case _ of
          ValueInt x -> ValueForeignFn case _ of
            ValueInt y -> ValueInt (x * y)
            _ -> ValueError "Expected Int"
          _ -> ValueError "Expected Int"
      )
  # evaluate "infixl 6 add as +"
  # evaluate "infixl 7 mul as *"
