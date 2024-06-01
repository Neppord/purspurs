module Interpreter where

import Prelude

import Data.Array (foldM, foldr, zipWith)
import Data.Identity (Identity)
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Tuple (Tuple(Tuple), snd)
import Parser (parse_declaration, parse_expression, parse_module)
import PursPurs.Declaration (Declaration(..))
import PursPurs.Expression (Binder(BinderConstructor, BinderError, BinderValue, BinderVariable, BinderWildcard), Branches, Expr(..))
import PursPurs.Fixity (Fixity(Infixl))
import PursPurs.Import (Import(..))
import PursPurs.Module (Module(Module, ModuleError))
import PursPurs.Value (Callable(..), Scope, Value(..), Values, empty_scope, lookup_operator, merge_scope, merge_values)
import Data.Array (any, findMap, foldr) as Array
import Data.List (fromFoldable, singleton, (:)) as List
import Data.List.Types (List(Nil)) as List
import Data.Map.Internal (empty, singleton) as Map
import PursPurs.Value (empty_scope, insert, insert_all, insert_operator, lookup_callable, lookup_value, names) as Value
import Data.Traversable (sequence)

type Interpreter m =
  { module_loader :: String -> m (Maybe String)
  }

evaluate_module :: forall m. Monad m => Interpreter m -> Module -> m (Maybe (Scope Expr))
evaluate_module _ ModuleError = pure Nothing
evaluate_module interpreter (Module imports declarations) = do
  imported_scope <- evaluate_imports interpreter imports
  pure $ foldr
    ( \declaration scope -> do
        scope' <- scope
        evaluate_declaration declaration scope'
    )
    (Just imported_scope)
    declarations

evaluate_imports :: forall m. Monad m => Interpreter m -> Array Import -> m (Scope Expr)
evaluate_imports interpreter imports = foldM
  (\scope import_ -> evaluate_import interpreter import_ <#> merge_scope scope)
  empty_scope
  imports

load_source_for_import :: forall m. Monad m => Interpreter m -> Import -> m (Maybe String)
load_source_for_import { module_loader } import_ = case import_ of
  Import name -> module_loader name
  ImportItems name _ -> module_loader name
  _ -> pure Nothing

evaluate_import :: forall m. Monad m => Interpreter m -> Import -> m (Scope Expr)
evaluate_import interpreter import_ = do
  text <- load_source_for_import interpreter import_
  case text of
    Nothing -> pure empty_scope
    Just source -> source
      # parse_module
      # evaluate_module interpreter
      <#> fromMaybe empty_scope

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
match_binder value = case _ of
  BinderVariable name -> Just (Map.singleton name value)
  BinderWildcard -> Just Map.empty
  BinderValue value_ | value == value_ -> Just Map.empty
  BinderValue _ -> Nothing
  BinderConstructor name binders -> value # match_constructor_binder name binders
  BinderError -> Just Map.empty

match_constructor_binder :: String -> Array Binder -> Value Expr -> Maybe (Values Expr)
match_constructor_binder name binders = case _ of
  ValueConstructor name_ values | name == name_ ->
    zipWith match_binder values binders # sequence <#> merge_values
  _ -> Nothing

evaluate :: forall a. Interpreter a -> String -> Scope Expr -> Scope Expr
evaluate i s env = case evaluate_declaration (parse_declaration s) env of
  Nothing -> env # Value.insert "_" (evaluate_expr env (parse_expression s))
  Just scope -> scope

evaluate_declaration :: Declaration -> Scope Expr -> Maybe (Scope Expr)
evaluate_declaration declaration scope = case declaration of
  DeclarationData _ constructors -> constructors
    # Array.foldr (\(Tuple key expr) -> Value.insert key (evaluate_expr scope expr)) scope
    # Value.insert "_" (ValueArray (constructors <#> snd <#> evaluate_expr scope))
    # Just
  DeclarationValue name expr -> do
    let value = evaluate_expr scope expr
    scope
      # Value.insert name value
      # Value.insert "_" value
      # Just
  DeclarationFixity fixity precedence function operator_name -> do
    let operation = scope # Value.lookup_callable function
    let operator = { operation, precedence, fixity }
    scope
      # Value.insert_operator operator_name operator
      # Just

  DeclarationSignature -> Just scope
  DeclarationError -> Nothing

print :: Scope Expr -> String
print env = env
  # Value.lookup_value "_"
  # show

names :: Scope Expr -> Array String
names = Value.names

fn2 :: (Value Expr -> Value Expr -> Value Expr) -> Value Expr
fn2 f = ValueCallable $ CallableForeignFn \a -> ValueCallable $ CallableForeignFn \b -> f a b

in_memory_interpreter :: Interpreter Identity
in_memory_interpreter = { module_loader: \_ -> pure Nothing }

evaluate_in_memory :: String -> Scope Expr -> Scope Expr
evaluate_in_memory expr scope = evaluate in_memory_interpreter expr scope

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
  # evaluate_in_memory "infixl 6 add as +"
  # evaluate_in_memory "infixl 6 sub as -"
  # evaluate_in_memory "infixl 7 mul as *"
  # evaluate_in_memory "infix 4 eq as =="
