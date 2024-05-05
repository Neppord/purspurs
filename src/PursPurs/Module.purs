module PursPurs.Module where

import Prelude
import PursPurs.Value (Scope)
import PursPurs.Declaration (Declaration)


data Module
    = Module (Array Declaration)
    | ModuleError