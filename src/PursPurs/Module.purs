module PursPurs.Module where

import Prelude
import PursPurs.Value (Scope)
import PursPurs.Declaration (Declaration)
import PursPurs.Import (Import)


data Module
    = Module (Array Import) (Array Declaration)
    | ModuleError