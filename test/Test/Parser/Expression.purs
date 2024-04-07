module Test.Parser.Expression where

import Prelude

import Data.Tuple.Nested ((/\))
import Parser (parse_expression)
import PursPurs.Expression (Binder(..), Expr(ExprApp, ExprCase, ExprIdentifier, ExprLambda, ExprLet, ExprValue), Value(ValueBoolean, ValueInt))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Data.Map (singleton) as Map

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
  it "parses case with literal branch" do
    let
      true_ = ExprValue (ValueBoolean true)
      false_ = ExprValue (ValueBoolean false)
      true_binder = BinderValue (ValueBoolean true)
      false_binder = BinderValue (ValueBoolean false)
    parse_expression
      """case true of
      true -> false
      false -> true
      """
      # shouldEqual ( ExprCase true_
              [ true_binder /\ false_
              , false_binder /\ true_
              ]
          )
  it "parses case with var binder" do
    let
      true_ = ExprValue (ValueBoolean true)
      x_binder = BinderVariable "x"
    parse_expression
      """case true of
      x -> x
      """
      # shouldEqual ( ExprCase true_ [ x_binder /\ x ] )
  it "parses let expression" do
    parse_expression """let x = 1 in x """
      # shouldEqual (ExprLet (Map.singleton "x" $ ExprValue $ ValueInt 1) x)
    parse_expression """let x = 1 in f x """
      # shouldEqual (ExprLet (Map.singleton "x" $ ExprValue $ ValueInt 1) (ExprApp f x))
