module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Interpreter (default_env, evaluate, print)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Spec (specReporter)
import Test.Spec.Runner (runSpec)
import Data.Array (foldl) as Array
import Test.Interpreter.Expression (spec) as Interpreter.Expression
import Test.Parser.Declaration (spec) as Parser.Declaration
import Test.Parser.Expression (spec) as Parser.Expression

run_program :: Array String -> String
run_program program = Array.foldl (\env decl -> evaluate decl env) default_env program
  # print

main :: Effect Unit
main = launchAff_ $ runSpec [ specReporter ] do
  Parser.Declaration.spec
  Parser.Expression.spec
  Interpreter.Expression.spec

  describe "evaluate program" do
    it "handles literal" do
      run_program [ "1" ] # shouldEqual "1"
    it "handles value declaration" do
      run_program [ "f x = 1" ] # shouldEqual "(\\x -> 1)"
    it "handles let in" do
      run_program [ "f y = y", "let x = 1 in f x" ] # shouldEqual "1"
    it "handles scope" do
      run_program [ "y = 1", "f y = y", "f 2" ] # shouldEqual "2"
      run_program [ "y = 1", "f y x = y", "f 2 3" ] # shouldEqual "2"
    it "handles session with data" do
      run_program [ "data Foo = Bar Int", "Bar 42" ] # shouldEqual "(Bar 42)"
    it "handles session case of" do
      run_program [ """f x = case x of
        false -> true
        x -> x
      """, "f true" ] # shouldEqual "true"
    it "handles session case of _" do
      run_program ["""case 1 of
        _ -> true
      """] # shouldEqual "true"
    it "handles session case of constructor" do
      run_program ["data Foo = A | B", """case B of
        A -> false
        B -> true
      """] # shouldEqual "true"

