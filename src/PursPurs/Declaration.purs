module PursPurs.Declaration where

import Prelude

import Data.Tuple (Tuple)
import PursPurs.Expression (Expr)
import Data.Array (intercalate) as Array
import PursPurs.Fixity (Fixity)

data Declaration
  = DeclarationError
  | DeclarationSignature
  | DeclarationValue String Expr
  | DeclarationFixity Fixity Int String String
  | DeclarationData String (Array (Tuple String Expr))

instance Show Declaration where
  show DeclarationError = "<Error>"
  show DeclarationSignature = "<Signature>"
  show (DeclarationFixity fixity precedence name op) =
    show fixity <> " " <> show precedence <> " " <> name <> " as " <> op
  show (DeclarationValue name value) = name <> " = " <> show value
  show (DeclarationData name constructors) = "data " <> name <> " = " <>
    (constructors <#> show # Array.intercalate " | ")

instance Eq Declaration where
  eq (DeclarationValue name value) (DeclarationValue name_ value_) = name_ == name && value == value_
  eq (DeclarationData name constructors) (DeclarationData name_ constructors_) =
    name_ == name && constructors == constructors_
  eq (DeclarationFixity fixity precedence name op) (DeclarationFixity fixity_ precedence_ name_ op_) =
    fixity == fixity_ && precedence == precedence_ && name == name_ && op == op_
  eq _ _ = false
