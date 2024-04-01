module Interpreter where

import Prelude

import PureScript.CST (RecoveredParserResult(..), parseExpr)
import PureScript.CST.Types (Expr(..), IntValue(..), Separated(Separated), Wrapped(Wrapped))
import Data.Maybe (Maybe(Nothing), Maybe(Just))
import Data.Tuple (snd)

type ExprState = {}
data Value
  = ValueVoid
  | ValueError
  | ValueBoolean Boolean
  | ValueInt IntValue
  | ValueChar Char
  | ValueNumber Number
  | ValueString String
  | ValueArray (Array Value)

instance Show Value where
  show ValueVoid = "Void"
  show ValueError = "<Error>"
  show (ValueBoolean b) = show b
  show (ValueChar b) = show b
  show (ValueString s) = show s
  show (ValueNumber s) = show s
  show (ValueArray a) = show a
  show (ValueInt i) = case i of
    SmallInt i' -> show i'
    BigInt s -> s
    BigHex h -> h

instance Eq Value where
  eq ValueVoid ValueVoid = true
  eq (ValueBoolean x) (ValueBoolean y) = x == y
  eq (ValueChar x) (ValueChar y) = x == y
  eq (ValueNumber x) (ValueNumber y) = x == y
  eq (ValueInt x) (ValueInt y) = x == y
  eq (ValueString x) (ValueString y) = x == y
  eq (ValueArray x) (ValueArray y) = x == y
  eq _ _ = false

interpret_expr :: String -> ExprState -> Value
interpret_expr expr _ = case parseExpr expr of
  ParseSucceeded e -> expressionToValue e
  _ -> ValueError
  where
  expressionToValue e = case e of
    ExprBoolean _ b -> ValueBoolean b
    ExprChar _ s -> ValueChar s
    ExprNumber _ s -> ValueNumber s
    ExprInt _ s -> ValueInt s
    ExprString _ s -> ValueString s
    ExprArray (Wrapped { value: Nothing }) -> ValueArray []
    ExprArray (Wrapped { value: Just (Separated { head, tail }) }) ->
      ValueArray
        ([ expressionToValue head ] <> (tail <#> snd <#> expressionToValue))
    _ -> ValueError