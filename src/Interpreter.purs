module Interpreter where

import Prelude

import Data.Map.Internal (Map)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple (Tuple(Tuple), snd)
import Data.Tuple.Nested ((/\))
import Parser (parse_declaration, parse_expression)
import PursPurs.Declaration (Declaration(..))
import PursPurs.Expression (Binder(BinderError, BinderValue, BinderVariable), Env, Expr(..), Value(..))
import Data.Array (find, foldr, fromFoldable) as Array
import Data.Map (keys) as Map
import Data.Map.Internal (fromFoldable, insert, lookup, union) as Map



evaluate_expr :: Env -> Expr -> Value
evaluate_expr _ (ExprValue value) = value
evaluate_expr env (ExprApp f a) =
  let
    argument = evaluate_expr env a
  in
    case evaluate_expr env f of
      ValueLambda key closure expr -> evaluate_expr (closure # Map.insert key argument) expr
      ValueForeignFn fn -> fn argument
      _ -> ValueError
evaluate_expr env (ExprIdentifier key) = case Map.lookup key env of
  Just v -> v
  Nothing -> ValueError
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
    _ -> ValueError
evaluate_expr env (ExprCase expr branches) = let
    value = evaluate_expr env expr
    Tuple binder expr_ = branches
        # (Array.find \(Tuple binder _) -> case binder of
            BinderVariable _ -> true
            BinderValue value_ -> value == value_
            BinderError -> true)
        # fromMaybe (Tuple BinderError ExprError)
    next_env = case binder of
        BinderValue _ -> env
        BinderVariable name -> env # Map.insert name value
        BinderError -> env
    in evaluate_expr next_env expr_
evaluate_expr _ _ = ValueError

evaluate :: String -> Env -> Env
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
  DeclarationError -> env # Map.insert "_" (evaluate_expr env (parse_expression s))

print :: Env -> String
print env = Map.lookup "_" env # maybe "<Error>" show

names :: Env -> Array String
names env = Map.keys env # Array.fromFoldable

default_env :: Map String Value
default_env = Map.fromFoldable
  [ "add" /\ ValueForeignFn case _ of
      ValueInt x -> ValueForeignFn case _ of
        ValueInt y -> ValueInt (x + y)
        _ -> ValueError
      _ -> ValueError
  , "mul" /\ ValueForeignFn case _ of
      ValueInt x -> ValueForeignFn case _ of
        ValueInt y -> ValueInt (x * y)
        _ -> ValueError
      _ -> ValueError
  ]