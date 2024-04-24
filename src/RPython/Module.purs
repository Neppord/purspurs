module RPython.Module where

import Prelude

import Data.Array (intercalate)
import Effect (Effect)
import RPython.Expression (translate_expression)
import PureScript.CST (RecoveredParserResult(..), parseModule) as CST
import PureScript.CST.Types (Binder, Binder(BinderVar), Declaration(..), Expr, Guarded(..), Ident(..), Module(..), ModuleBody(..), Name(..), Where(..)) as CST
import Data.Ord (lessThan)
import PureScript.CST.Types (Ident(..)) as Cst

compile_module :: String -> String
compile_module source = case CST.parseModule source of
  CST.ParseSucceeded (CST.Module { body: CST.ModuleBody { decls } }) ->
    """from __future__ import print_function
""" <> (decls <#> compile_declaration # intercalate "\n\n") <>
      """


def entry_point(args):
    main()
    return 0


def target(*args):
    return entry_point, None
"""
  _ -> ""

compile_declaration :: CST.Declaration Void -> String
compile_declaration = case _ of
  CST.DeclValue
    { name: CST.Name { name: CST.Ident name }
    , binders
    , guarded: CST.Unconditional _ (CST.Where { expr })
    } -> compile_value_declaration name binders expr
  _ -> """# skipping declaration"""

compile_value_declaration
  :: String -> Array (CST.Binder Void) -> CST.Expr Void -> String
compile_value_declaration name binders expr =
  let
    parameters = binders <#> case _ of
            CST.BinderVar (CST.Name {name: CST.Ident parameter_name}) -> parameter_name
            _ -> "?"
        # intercalate ", "
    body = expr # translate_expression # show
  in
    "def " <> name <> "(" <> parameters <> "):\n    " <> body