module Interpreter where

import Prelude

import Data.Maybe (Maybe(..), isNothing)
import Data.Tuple (Tuple(Tuple), snd)
import Parser (parse_declaration, parse_expression)
import PursPurs.Declaration (Declaration(..))
import PursPurs.Expression (Binder(BinderConstructor, BinderError, BinderValue, BinderVariable, BinderWildcard), Branches, Expr(..))
import PursPurs.Fixity (Fixity(Infixl))
import PursPurs.Module (Module(Module, ModuleError))
import PursPurs.Value (Callable(..), Scope, Value(..), Values, empty_scope, lookup_operator)
import Data.Array (any, catMaybes, findMap, foldr) as Array
import Data.List (fromFoldable, singleton, (:)) as List
import Data.List.Types (List(Nil)) as List
import Data.Map.Internal (empty, singleton, union) as Map
import PursPurs.Value (empty_scope, insert, insert_all, insert_operator, lookup_value, lookup_callable, names) as Value
import Data.Array (foldr)

type Interpreter m =
    { module_loader :: String -> m (Maybe String)
    }

evaluate_module ::forall m. Monad m => Interpreter m -> Module -> m (Maybe (Scope Expr))
evaluate_module _ ModuleError = pure Nothing
evaluate_module _ (Module _ declarations) = pure $ foldr
 (\declaration scope -> do
    scope' <- scope
    evaluate_declaration declaration scope')
 (Just empty_scope)
 declarations

evaluate_call :: Callable Expr -> Value Expr -> Value Expr
evaluate_call (CallableError msg) _ = ValueError msg
evaluate_call (CallableForeignFn fn) arg = fn arg
evaluate_call (CallableLambda key closure expr) arg =
  evaluate_expr (closure # Value.insert key arg) expr

evaluate_expr :: Scope Expr -> Expr -> Value Expr
evaluate_expr _ (ExprValue value) = value
evaluate_expr env (ExprOp l tail) = evaluate_operators env l tail
evaluate_expr env (ExprApp f a) = case evaluate_expr env f of
  ValueCallable c -> evaluate_call c (evaluate_expr env a)
  _ -> ValueError (show f <> " is not callable")
evaluate_expr env (ExprIdentifier key) = env # Value.lookup_value key
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

evaluate_operators :: Scope Expr -> Expr -> Array (Tuple String Expr) -> Value Expr
evaluate_operators env l rest =
  operator_call env l rest # evaluate_expr env

operator_call :: Scope Expr -> Expr -> Array (Tuple String Expr) -> Expr
operator_call scope first rest =
  let
    queue = rest
      <#> (\(Tuple str expr) -> Tuple (lookup_operator str scope) expr)
      # List.fromFoldable

    -- Error condition, none of thes should be posible, but we dont know yet.
    pop List.Nil List.Nil = ExprError
    pop (_ List.: _) List.Nil = ExprError
    pop (_ List.: _) (_ List.: List.Nil) = ExprError
    pop List.Nil (_ List.: (_ List.: _)) = ExprError
    -- Done
    pop List.Nil (expr List.: List.Nil) = expr
    -- pop the stack
    pop ({ operation } List.: ops) (a List.: (b List.: expressions)) =
      let
        top = ExprApp (ExprApp (ExprValue (ValueCallable operation)) a) b
      in
        pop ops (top List.: expressions)

    go List.Nil ops expressions = pop ops expressions
    go ((Tuple Nothing _) List.: _) _ _ = ExprError
    go ((Tuple (Just op) expression) List.: q) List.Nil expressions =
      go q (List.singleton op) (expression List.: expressions)
    go qs@((Tuple (Just op@{ precedence: new, fixity }) expr) List.: q) ops@({ precedence: current } List.: _) exprs =
      if new < current then go qs List.Nil (List.singleton (pop ops exprs))
      else if new == current && fixity == Infixl then go qs List.Nil (List.singleton (pop ops exprs))
      else go q (op List.: ops) (expr List.: exprs)
  in
    go queue List.Nil (List.singleton first)

evaluate_case_of :: Value Expr -> Branches -> Scope Expr -> Value Expr
evaluate_case_of (ValueError msg) _ _ = ValueError msg
evaluate_case_of value branches env =
  if branches # contains_any_errors then ValueError "Bad case of"
  else case branches # Array.findMap \(Tuple b expr_) -> match_binder value b <#> Tuple expr_ of
    Nothing -> ValueError "No matching branch in case of"
    Just (Tuple expr_ values) -> evaluate_expr (Value.insert_all values env) expr_

contains_any_errors :: Branches -> Boolean
contains_any_errors branches = branches # Array.any case _ of
  Tuple BinderError _ -> true
  Tuple _ ExprError -> true
  _ -> false

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

evaluate :: String -> Scope Expr -> Scope Expr
evaluate s env = case evaluate_declaration (parse_declaration s) env of
    Nothing -> env # Value.insert "_" (evaluate_expr env (parse_expression s))
    Just scope -> scope


evaluate_declaration:: Declaration -> Scope Expr -> Maybe (Scope Expr)
evaluate_declaration declaration env = case declaration of
  DeclarationData _ constructors -> constructors
    # Array.foldr (\(Tuple key expr) -> Value.insert key (evaluate_expr env expr)) env
    # Value.insert "_" (ValueArray (constructors <#> snd <#> evaluate_expr env))
    # Just
  DeclarationValue name expr ->
    let
      value = evaluate_expr env expr
    in
      env
        # Value.insert name value
        # Value.insert "_" value
        # Just
  DeclarationFixity fixity precedence function operator_name ->
    let
      operation = env # Value.lookup_callable function
      operator =
        { operation
        , precedence
        , fixity
        }
    in
      env
        # Value.insert_operator operator_name operator
        # Just

  DeclarationError -> Nothing

print :: Scope Expr -> String
print env = env
  # Value.lookup_value "_"
  # show

names :: Scope Expr -> Array String
names = Value.names

fn2 :: (Value Expr -> Value Expr -> Value Expr) -> Value Expr
fn2 f = ValueCallable $ CallableForeignFn \a -> ValueCallable $ CallableForeignFn \b -> f a b

default_env :: Scope Expr
default_env = Value.empty_scope
  # Value.insert "add"
      ( fn2 case _, _ of
          ValueInt x, ValueInt y -> ValueInt (x + y)
          _, _ -> ValueError "Expected Int"
      )
  # Value.insert "sub"
      ( fn2 case _, _ of
          ValueInt x, ValueInt y -> ValueInt (x - y)
          _, _ -> ValueError "Expected Int"
      )
  # Value.insert "mul"
      ( fn2 case _, _ of
          ValueInt x, ValueInt y -> ValueInt (x * y)
          _, _ -> ValueError "Expected Int"
      )
  # Value.insert "eq" (fn2 \x y -> ValueBoolean (x == y))
  # evaluate "infixl 6 add as +"
  # evaluate "infixl 6 sub as -"
  # evaluate "infixl 7 mul as *"
  # evaluate "infix 4 eq as =="
