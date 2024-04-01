module Parser where

import Prelude

import Data.Int (fromString)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (snd)
import PureScript.CST (RecoveredParserResult(ParseSucceeded), parseDecl, parseExpr)
import PureScript.CST.Types (AppSpine(AppTerm), Binder(BinderVar), Declaration(DeclValue), Expr(..), Guarded(Unconditional), Ident(Ident), IntValue(..), Name(Name), QualifiedName(QualifiedName), Separated(Separated), Where(Where), Wrapped(Wrapped)) as CST
import Data.Array (intercalate)
import Data.Array.NonEmpty.Internal (NonEmptyArray(NonEmptyArray))
import PureScript.CST.Types (Ident(Ident))

data Declaration
  = DeclarationError
  | DeclarationValue String Expr

instance Show Declaration where
  show DeclarationError = "<Error>"
  show (DeclarationValue name value) = name <> " = " <> show value

instance Eq Declaration where
  eq (DeclarationValue name value) (DeclarationValue name_ value_) = name_ == name && value == value_
  eq _ _ = false

data Expr
  = ExprError
  | ExprIdentifier String
  | ExprValue Value
  | ExprArray (Array Expr)
  | ExprApp Expr Expr

instance Show Expr where
  show ExprError = "<Error>"
  show (ExprValue value) = show value
  show (ExprApp f x) = "(" <> (show f) <> " " <> (show x) <> ")"
  show (ExprIdentifier identifier) = identifier
  show (ExprArray array) = "[" <> intercalate ", " (array <#> show) <> "]"

instance Eq Expr where
  eq (ExprIdentifier x) (ExprIdentifier y) = x == y
  eq (ExprValue x) (ExprValue y) = x == y
  eq (ExprApp f x) (ExprApp g y) = x == y && f == g
  eq (ExprArray x) (ExprArray y) = x == y
  eq _ _ = false

data Value
  = ValueVoid
  | ValueError
  | ValueBoolean Boolean
  | ValueInt Int
  | ValueChar Char
  | ValueNumber Number
  | ValueString String
  | ValueArray (Array Value)
  | ValueLambda String Expr

instance Show Value where
  show ValueVoid = "Void"
  show ValueError = "<Error>"
  show (ValueBoolean b) = show b
  show (ValueChar b) = show b
  show (ValueString s) = show s
  show (ValueNumber s) = show s
  show (ValueArray a) = show a
  show (ValueInt i) = show i
  show (ValueLambda param expr) = "(\\" <> param <> " -> " <> show expr <> ")"

instance Eq Value where
  eq ValueVoid ValueVoid = true
  eq (ValueBoolean x) (ValueBoolean y) = x == y
  eq (ValueChar x) (ValueChar y) = x == y
  eq (ValueNumber x) (ValueNumber y) = x == y
  eq (ValueInt x) (ValueInt y) = x == y
  eq (ValueString x) (ValueString y) = x == y
  eq (ValueArray x) (ValueArray y) = x == y
  eq (ValueLambda param expr) (ValueLambda param_ expr_) = param == param_ && expr == expr_
  eq _ _ = false

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
      ExprValue (ValueLambda name (expression_from_CST body))
    CST.ExprApp f (NonEmptyArray [ CST.AppTerm a ]) -> ExprApp (expression_from_CST f) (expression_from_CST a)
    CST.ExprParens (CST.Wrapped { value: cst }) -> expression_from_CST cst
    _ -> ExprError

parse_declaration :: String -> Declaration
parse_declaration declaration = case parseDecl declaration of
  ParseSucceeded decl -> fromCST decl
  _ -> DeclarationError
  where
  fromCST = case _ of
    CST.DeclValue
      { name: CST.Name { name: Ident name }
      , guarded: CST.Unconditional _ (CST.Where {expr})
      } ->
      DeclarationValue name (expression_from_CST expr)
    _ -> DeclarationError