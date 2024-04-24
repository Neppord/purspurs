module Print where

import Prelude
import Effect (Effect)

foreign import print :: String -> Effect Unit

do_it :: String -> Effect Unit
do_it msg = print (msg <> "!")

main :: Effect Unit
main = do_it "hello world"