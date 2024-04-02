module Parser where

import Prelude

import Data.Array (intercalate)
import Data.Array.NonEmpty.Internal (NonEmptyArray(NonEmptyArray))
import Data.Int (fromString)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(Tuple), snd)
import Data.Tuple.Nested ((/\))
import PureScript.CST (RecoveredParserResult(ParseSucceeded), parseDecl, parseExpr)
import Data.Array (foldl, foldr, intercalate, mapWithIndex) as Array
import PureScript.CST.Types (AppSpine(AppTerm), Binder(BinderVar), DataCtor(DataCtor), Declaration(DeclData, DeclValue), Expr(..), Guarded(Unconditional), Ident(Ident), IntValue(..), Name(Name), Proper(Proper), QualifiedName(QualifiedName), Separated(Separated), Where(Where), Wrapped(Wrapped)) as CST

data Declaration
  = DeclarationError
  | DeclarationValue String Expr
  | DeclarationData String (Array (Tuple String Value))

instance Show Declaration where
  show DeclarationError = "<Error>"
  show (DeclarationValue name value) = name <> " = " <> show value
  show (DeclarationData name constructors) = "data " <> name <> " = " <>
    (constructors <#> show # Array.intercalate " | ")

instance Eq Declaration where
  eq (DeclarationValue name value) (DeclarationValue name_ value_) = name_ == name && value == value_
  eq (DeclarationData name constructors) (DeclarationData name_ constructors_) =
    name_ == name && constructors == constructors_
  eq _ _ = false

data Expr
  = ExprError
  | ExprIdentifier String
  | ExprValue Value
  | ExprArray (Array Expr)
  | ExprApp Expr Expr
  | ExprConstructor String (Array Expr)

instance Show Expr where
  show ExprError = "<Error>"
  show (ExprValue value) = show value
  show (ExprApp f x) = "(" <> (show f) <> " " <> (show x) <> ")"
  show (ExprIdentifier identifier) = identifier
  show (ExprArray array) = "[" <> intercalate ", " (array <#> show) <> "]"
  show (ExprConstructor name array) = ([ name ] <> (array <#> show <#> \x -> "(" <> x <> ")")) # Array.intercalate " "

instance Eq Expr where
  eq (ExprIdentifier x) (ExprIdentifier y) = x == y
  eq (ExprValue x) (ExprValue y) = x == y
  eq (ExprApp f x) (ExprApp g y) = x == y && f == g
  eq (ExprArray x) (ExprArray y) = x == y
  eq (ExprConstructor name array) (ExprConstructor name_ array_) = name == name_ && array == array_
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
  | ValueConstructor String (Array Value)

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
  show (ValueConstructor name values) =
    if values == [] then name
    else
      "(" <> name <> " " <> (values <#> show # Array.intercalate " ") <> ")"

instance Eq Value where
  eq ValueVoid ValueVoid = true
  eq (ValueBoolean x) (ValueBoolean y) = x == y
  eq (ValueChar x) (ValueChar y) = x == y
  eq (ValueNumber x) (ValueNumber y) = x == y
  eq (ValueInt x) (ValueInt y) = x == y
  eq (ValueString x) (ValueString y) = x == y
  eq (ValueArray x) (ValueArray y) = x == y
  eq (ValueLambda param expr) (ValueLambda param_ expr_) = param == param_ && expr == expr_
  eq (ValueConstructor name values) (ValueConstructor name_ values_) = name == name_ && values == values_
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
  CST.ExprApp function (NonEmptyArray arguments) ->
    arguments # Array.foldl
      ( \f -> case _ of
          CST.AppTerm a -> ExprApp f (expression_from_CST a)
          _ -> ExprError
      )
      (expression_from_CST function)
  CST.ExprConstructor (CST.QualifiedName {name: CST.Proper name}) -> ExprIdentifier name
  CST.ExprParens (CST.Wrapped { value: cst }) -> expression_from_CST cst
  _ -> ExprError

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
                [] -> c /\ ValueConstructor c []
                f -> let
                    parameters = f # Array.mapWithIndex \i _ -> ("$" <> show i)
                    constructor = ExprConstructor c (parameters <#> ExprIdentifier)
                  in
                    c /\ case parameters # Array.foldr (\p b -> ExprValue (ValueLambda p b)) constructor of
                        ExprValue x -> x
                        _ -> ValueError

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
              \body -> ExprValue (ValueLambda param body)
            _ -> \_ -> ExprError
          base_expression
      in
        DeclarationValue name expression
    _ -> DeclarationError