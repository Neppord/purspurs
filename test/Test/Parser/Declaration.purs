module Test.Parser.Declaration where

import Prelude

import Data.Tuple.Nested ((/\))
import Parser (parse_declaration)
import PursPurs.Declaration (Declaration(DeclarationData, DeclarationFixity, DeclarationValue), Fixity(Infixl))
import PursPurs.Expression (Expr(ExprConstructor, ExprIdentifier, ExprLambda, ExprValue), Value(ValueInt))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "declaration parser" do
  it "parses data declaration with one constructor" do
    parse_declaration "data Cmd = Noop"
      # shouldEqual (DeclarationData "Cmd" [ "Noop" /\ ExprConstructor "Noop" [] ])
  it "parses data declaration with one constructor with two parameter" do
    parse_declaration "data Cmd = Add Int Int"
      # shouldEqual
          ( DeclarationData "Cmd"
              [ "Add" /\
                  ( ExprLambda "$0"
                      $ ExprLambda "$1"
                      $ ExprConstructor "Add" [ ExprIdentifier "$0", ExprIdentifier "$1" ]
                  )
              ]
          )
  it "parses data declaration with two constructor" do
    parse_declaration "data Command = Noop | Quit"
      # shouldEqual
          ( DeclarationData "Command"
              [ "Noop" /\ ExprConstructor "Noop" []
              , "Quit" /\ ExprConstructor "Quit" []
              ]
          )
  it "parses value declaration with no parameter" do
    let
      expression = ExprValue (ValueInt 42)
      declaration = DeclarationValue "x" expression
    parse_declaration "x = 42" # shouldEqual declaration
  it "parses value declaration with one parameter" do
    let
      expression = ExprLambda "x" $ ExprValue $ ValueInt 42
      declaration = DeclarationValue "f" expression
    parse_declaration "f x = 42" # shouldEqual declaration
  it "parses value declaration with two parameter" do
    let
      expression = ExprLambda "x" $ ExprLambda "y" $ ExprValue $ ValueInt 42
      declaration = DeclarationValue "f" expression
    parse_declaration "f x y = 42" # shouldEqual declaration
  it "parses fixity" do
    parse_declaration "infixl 6 add as +" # shouldEqual (DeclarationFixity Infixl 6 "add" "+")
