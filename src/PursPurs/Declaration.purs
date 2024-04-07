module PursPurs.Declaration where

import Prelude

import Data.Tuple (Tuple)
import PursPurs.Expression (Expr)
import Data.Array (intercalate) as Array

data Declaration
  = DeclarationError
  | DeclarationValue String Expr
  | DeclarationData String (Array (Tuple String Expr))

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
