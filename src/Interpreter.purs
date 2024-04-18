module Interpreter where

import Prelude

import Data.Maybe (Maybe(..), isNothing)
import Data.Tuple (Tuple(Tuple), snd)
import Parser (parse_declaration, parse_expression)
import PursPurs.Declaration (Declaration(..))
import PursPurs.Expression (Binder(BinderConstructor, BinderError, BinderValue, BinderVariable, BinderWildcard), Branches, Expr(..))
import PursPurs.Value (Callable(..), Env, Value(..), Values)
import Data.Array (any, catMaybes, findMap, foldr) as Array
import Data.Map.Internal (empty, singleton, union) as Map
import PursPurs.Value (empty_env, insert, insert_all, insert_operator, lookup, lookup_callable, lookup_operator, names) as Value

evaluate_call :: Callable Expr -> Value Expr -> Value Expr
evaluate_call (CallableError msg) _ = ValueError msg
evaluate_call (CallableForeignFn fn) arg = fn arg
evaluate_call (CallableLambda key closure expr) arg =
  evaluate_expr (closure # Value.insert key arg) expr

evaluate_expr :: Env Expr -> Expr -> Value Expr
evaluate_expr _ (ExprValue value) = value
evaluate_expr env (ExprOp l op r) = case env # Value.lookup_operator op of
  Just { operation } -> case evaluate_call operation (evaluate_expr env l) of
    ValueCallable c_ -> evaluate_call c_ (evaluate_expr env r)
    _ -> ValueError (op <> " only takes one argument, but must take two")
  _ -> ValueError ("Cant find " <> op <> " in scope")

evaluate_expr env (ExprApp f a) = case evaluate_expr env f of
  ValueCallable c -> evaluate_call c (evaluate_expr env a)
  _ -> ValueError (show f <> " is not callable")
evaluate_expr env (ExprIdentifier key) = env # Value.lookup key
evaluate_expr env (ExprArray values) = ValueArray (values <#> evaluate_expr env)
evaluate_expr env (ExprConstructor name values) = ValueConstructor name (values <#> evaluate_expr env)
evaluate_expr env (ExprLet m expr) =
  evaluate_expr (Value.insert_all (m <#> evaluate_expr env) env) expr
evaluate_expr env (ExprLambda p e) = ValueCallable (CallableLambda p env e)
evaluate_expr env (ExprIfElse i t e) = case evaluate_expr env i of
  ValueBoolean true -> evaluate_expr env t
  ValueBoolean false -> evaluate_expr env e
  _ -> ValueError "expected boolean in if"
evaluate_expr env (ExprCase expr branches) = env # evaluate_case_of (evaluate_expr env expr) branches
evaluate_expr _ _ = ValueError "?"

evaluate_case_of :: Value Expr -> Branches -> Env Expr -> Value Expr
evaluate_case_of (ValueError msg) _ _ = ValueError msg
evaluate_case_of value branches env =
  let
    bad_branch = branches # Array.any case _ of
        Tuple BinderError _ -> true
        Tuple _ ExprError -> true
        _ -> false
  in
    if bad_branch then ValueError "Bad case of"
    else case branches # Array.findMap \(Tuple b expr_) -> match_binder value b <#> Tuple expr_ of
      Nothing -> ValueError "No matching branch in case of"
      Just (Tuple expr_ values) -> evaluate_expr (Value.insert_all values env) expr_

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
      operation = env # Value.lookup_callable function
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
      ( ValueCallable $ CallableForeignFn case _ of
          ValueInt x -> ValueCallable $ CallableForeignFn case _ of
            ValueInt y -> ValueInt (x + y)
            _ -> ValueError "Expected Int"
          _ -> ValueError "Expected Int"
      )
  # Value.insert "mul"
      ( ValueCallable $ CallableForeignFn case _ of
          ValueInt x -> ValueCallable $ CallableForeignFn case _ of
            ValueInt y -> ValueInt (x * y)
            _ -> ValueError "Expected Int"
          _ -> ValueError "Expected Int"
      )
  # Value.insert "eq"
      ( ValueCallable $ CallableForeignFn case _ of
          ValueInt x -> ValueCallable $ CallableForeignFn case _ of
            ValueInt y -> ValueBoolean (x == y)
            _ -> ValueError "Expected Int"
          _ -> ValueError "Expected Int"
      )
  # evaluate "infixl 6 add as +"
  # evaluate "infixl 7 mul as *"
  # evaluate "infix 4 eq as =="
