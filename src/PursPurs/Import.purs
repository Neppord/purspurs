module PursPurs.Import where

import Prelude


data Import
    = Import String
    | ImportAs String String
    | ImportItems String (Array Item)
    | ImportItemsAs String (Array Item) String
    | ImportHiding String (Array Item)
    | ImportAsHiding String (Array Item)
    | Error

data Item
  = Value String
  | Op String
  | Type String
  | TypeWithAllMembers String
  | TypeWithMembers String (Array String)
  | TypeOp String
  | Class String
