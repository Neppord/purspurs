module Test.Parser.Expression where

import Prelude

import Parser (Expr(ExprApp, ExprIdentifier, ExprLambda, ExprLet, ExprValue), Expr(ExprIfElse), Expr(ExprCase), Value(ValueInt), Value(ValueBoolean), parse_expression)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Data.Map (singleton) as Map
import Data.Map.Internal (fromFoldable) as Map
import Data.Tuple.Nested ((/\))

spec :: Spec Unit
spec = describe "expression parser" do
  let
    f = ExprIdentifier "f"
    x = ExprIdentifier "x"
    y = ExprIdentifier "y"
  it "parses identifiers" do
    parse_expression "x" # shouldEqual x
  it "parses lambda" do
    parse_expression "\\x -> x" # shouldEqual (ExprLambda "x" x)
  it "parses app" do
    parse_expression "f x" # shouldEqual (ExprApp f x)
  it "parses chained app" do
    parse_expression "f x y" # shouldEqual (ExprApp (ExprApp f x) y)
  it "parses case" do
    let
      true_ = ExprValue (ValueBoolean true)
      false_ = ExprValue (ValueBoolean false)
    parse_expression
      """case true of
      true -> false
      false -> true
      """
      # shouldEqual ( ExprCase true_
              [ true_ /\ false_
              , false_ /\ true_
              ]
          )
  it "parses let expression" do
    parse_expression """let x = 1 in x """
      # shouldEqual (ExprLet (Map.singleton "x" $ ExprValue $ ValueInt 1) x)
    parse_expression """let x = 1 in f x """
      # shouldEqual (ExprLet (Map.singleton "x" $ ExprValue $ ValueInt 1) (ExprApp f x))
