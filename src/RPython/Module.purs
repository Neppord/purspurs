module RPython.Module where

import Prelude

import Data.Array (intercalate)
import Effect (Effect)
import RPython.Expression (translate_expression)
import PureScript.CST (RecoveredParserResult(..), parseModule) as CST
import PureScript.CST.Types (Declaration(..), Guarded(..), Ident(..), Module(..), ModuleBody(..), Name(..), Where(..)) as CST

compile_module :: String -> String
compile_module source = case CST.parseModule source of
  CST.ParseSucceeded (CST.Module { body: CST.ModuleBody { decls } }) ->
    """from __future__ import print_function
"""
      <>
        ( decls
            <#> case _ of
              CST.DeclValue { name: CST.Name { name: CST.Ident name }, guarded: CST.Unconditional _ (CST.Where { expr }) } ->
                "def " <> name <> "():\n    " <> (expr # translate_expression # show)
              _ -> """# skipping declaration"""
            # intercalate "\n\n"
        )
      <>
        """


def entry_point(*args):
    main()
    return 0


def target(*args):
    return entry_point, None
"""
  _ -> ""
