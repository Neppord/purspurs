module Test.RPython.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import RPython.Expression (compile_expression_from_string)
import RPython.Module (compile_module)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Spec (specReporter)
import Test.Spec.Runner (runSpec)
import PureScript.CST as CST
import PureScript.CST.Types as CST

main :: Effect Unit
main = launchAff_ $ runSpec [ specReporter ] do
  describe "rpython compiler" do
    it "compiles simple program" do
      compile_module
        """module Main where
main = print "hello world"
        """ # shouldEqual
        """from __future__ import print_function
def main():
    print("hello world")


def entry_point(*args):
    main()
    return 0


def target(*args):
    return entry_point, None
"""
    it "compiles values" do
      compile_expression_from_string "1" # shouldEqual "1"
      compile_expression_from_string "true" # shouldEqual "True"
      compile_expression_from_string "4.2" # shouldEqual "4.2"
      compile_expression_from_string "foo" # shouldEqual "foo"
      compile_expression_from_string "Foo" # shouldEqual "Foo"
      compile_expression_from_string "'a'" # shouldEqual "\"a\""
      compile_expression_from_string "\"a\"" # shouldEqual "\"a\""
      compile_expression_from_string "bar.foo" # shouldEqual "bar.foo"
      compile_expression_from_string "bar.foo.baz" # shouldEqual "bar.foo.baz"
      compile_expression_from_string "[]" # shouldEqual "[]"
      compile_expression_from_string "[1]" # shouldEqual "[1]"
      compile_expression_from_string "[1, 2]" # shouldEqual "[1, 2]"

    it "compiles expressions" do
      compile_expression_from_string "1 + 2" # shouldEqual "(1 + 2)"
      compile_expression_from_string "1 + 2 * 3" # shouldEqual "(1 + (2 * 3))"
      compile_expression_from_string "1 * 2 + 3" # shouldEqual "((1 * 2) + 3)"
      compile_expression_from_string "\"Hello \" <> \"World\"" # shouldEqual "(\"Hello \" + \"World\")"
