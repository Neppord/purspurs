module Parser where

import Prelude

import Data.Array.NonEmpty.Internal (NonEmptyArray(NonEmptyArray))
import Data.Int (fromString)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(Tuple), snd)
import Data.Tuple.Nested ((/\))
import PureScript.CST (RecoveredParserResult(ParseSucceeded), parseDecl, parseExpr)
import PursPurs.Declaration (Declaration(DeclarationData, DeclarationError, DeclarationValue))
import PursPurs.Expression (Expr(..), Value(..))
import Data.Array (foldl, foldr, mapWithIndex) as Array
import PureScript.CST.Types (AppSpine(..), Binder(..), DataCtor(..), Declaration(..), Expr(..), Guarded(..), Ident(..), IntValue(..), LetBinding(LetBindingName), Name(..), Proper(..), QualifiedName(..), Separated(..), Where(..), Wrapped(..)) as CST
import Data.Map.Internal (fromFoldable) as Map

parse_expression :: String -> Expr
parse_expression expr = case parseExpr expr of
  ParseSucceeded e -> expression_from_CST e
  _ -> ExprError

expression_from_CST :: CST.Expr Void -> Expr
expression_from_CST e = case e of
  CST.ExprBoolean _ b -> ExprValue $ ValueBoolean b
  CST.ExprChar _ s -> ExprValue $ ValueChar s
  CST.ExprNumber _ s -> ExprValue $ ValueNumber s
  CST.ExprInt _ s -> ExprValue case s of
    CST.SmallInt i -> ValueInt i
    CST.BigInt i -> fromString i # maybe ValueError ValueInt
    CST.BigHex i -> fromString i # maybe ValueError ValueInt
  CST.ExprString _ s -> ExprValue $ ValueString s
  CST.ExprArray (CST.Wrapped { value: Nothing }) -> ExprValue $ ValueArray []
  CST.ExprArray (CST.Wrapped { value: Just (CST.Separated { head, tail }) }) ->
    ExprArray ([ expression_from_CST head ] <> (tail <#> snd <#> expression_from_CST))
  CST.ExprIdent (CST.QualifiedName { name: CST.Ident name }) -> ExprIdentifier name
  CST.ExprLambda { binders: NonEmptyArray [ CST.BinderVar (CST.Name { name: CST.Ident name }) ], body } ->
    ExprLambda name (expression_from_CST body)
  CST.ExprApp function (NonEmptyArray arguments) ->
    arguments # Array.foldl
      ( \f -> case _ of
          CST.AppTerm a -> ExprApp f (expression_from_CST a)
          _ -> ExprError
      )
      (expression_from_CST function)
  CST.ExprConstructor (CST.QualifiedName { name: CST.Proper name }) -> ExprIdentifier name
  CST.ExprParens (CST.Wrapped { value: cst }) -> expression_from_CST cst
  CST.ExprIf { cond, true: t, false: f } -> ExprIfElse
    (expression_from_CST cond)
    (expression_from_CST t)
    (expression_from_CST f)
  CST.ExprCase { head: CST.Separated { head }, branches: NonEmptyArray branches } ->
    ExprCase (expression_from_CST head)
      ( branches <#>
          ( \(Tuple (CST.Separated { head: binder }) guarded) -> case guarded of
              CST.Unconditional _ (CST.Where { expr }) ->
                expression_from_binder binder /\ expression_from_CST expr
              _ -> ExprError /\ ExprError
          )
      )
  CST.ExprLet { bindings: NonEmptyArray bindings, body } ->
    ExprLet
      ( bindings
          <#> case _ of
            CST.LetBindingName
              { name: CST.Name { name: CST.Ident name }
              , guarded: CST.Unconditional _ (CST.Where { expr })
              } -> name /\ expression_from_CST expr
            _ -> "???" /\ ExprError
          # Map.fromFoldable
      )
      (expression_from_CST body)
  _ -> ExprError

expression_from_binder :: CST.Binder Void -> Expr
expression_from_binder (CST.BinderBoolean _ value) = ExprValue $ ValueBoolean value
expression_from_binder _ = ExprError

parse_declaration :: String -> Declaration
parse_declaration declaration = case parseDecl declaration of
  ParseSucceeded decl -> fromCST decl
  _ -> DeclarationError
  where
  fromCST = case _ of
    CST.DeclData { name: CST.Name { name: CST.Proper name } } a -> DeclarationData name case a of
      Nothing -> []
      Just (Tuple _ (CST.Separated { head, tail })) ->
        let
          all = [ head ] <> (tail <#> snd)
        in
          all <#>
            ( \(CST.DataCtor { name: CST.Name { name: CST.Proper c }, fields }) -> case fields of
                [] -> c /\ ExprConstructor c []
                f ->
                  let
                    parameters = f # Array.mapWithIndex \i _ -> "$" <> show i
                    constructor = ExprConstructor c (parameters <#> ExprIdentifier)
                  in
                    c /\ (Array.foldr ExprLambda constructor parameters :: Expr)

            )

    CST.DeclValue
      { name: CST.Name { name: CST.Ident name }
      , binders
      , guarded: CST.Unconditional _ (CST.Where { expr })
      } ->
      let
        base_expression = expression_from_CST expr
        expression = binders # Array.foldr
          case _ of
            CST.BinderVar (CST.Name { name: CST.Ident param }) ->
              \body -> ExprLambda param body
            _ -> \_ -> ExprError
          base_expression
      in
        DeclarationValue name expression
    _ -> DeclarationError