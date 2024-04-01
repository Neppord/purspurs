module Parser where

import Prelude

import Data.Int (fromString)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (snd)
import PureScript.CST (RecoveredParserResult(ParseSucceeded), parseExpr)
import PureScript.CST.Types (Expr(..), IntValue(..), Separated(Separated), Wrapped(Wrapped)) as CST

data Value
  = ValueVoid
  | ValueError
  | ValueBoolean Boolean
  | ValueInt Int
  | ValueChar Char
  | ValueNumber Number
  | ValueString String
  | ValueArray (Array Value)

data Expr
  = ExprError
  | ExprValue Value
  | ExprArray (Array Expr)

instance Show Value where
  show ValueVoid = "Void"
  show ValueError = "<Error>"
  show (ValueBoolean b) = show b
  show (ValueChar b) = show b
  show (ValueString s) = show s
  show (ValueNumber s) = show s
  show (ValueArray a) = show a
  show (ValueInt i) = show i

instance Eq Value where
  eq ValueVoid ValueVoid = true
  eq (ValueBoolean x) (ValueBoolean y) = x == y
  eq (ValueChar x) (ValueChar y) = x == y
  eq (ValueNumber x) (ValueNumber y) = x == y
  eq (ValueInt x) (ValueInt y) = x == y
  eq (ValueString x) (ValueString y) = x == y
  eq (ValueArray x) (ValueArray y) = x == y
  eq _ _ = false

parse_expression :: String -> Expr
parse_expression expr = case parseExpr expr of
  ParseSucceeded e -> fromCST e
  _ -> ExprError
  where
  fromCST e = case e of
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
      ExprArray ([ fromCST head ] <> (tail <#> snd <#> fromCST))
    _ -> ExprError
