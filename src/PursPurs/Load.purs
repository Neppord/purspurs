module PursPurs.Load where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(Just, Nothing))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class.Console (logShow)
import Milkis (URL(URL), fetch, getMethod, json, makeHeaders)
import Milkis.Impl (FetchImpl)
import Milkis.Impl.Node (nodeFetch)
import Yoga.JSON (E, read)
import Data.Array (find) as Array

type Item =
  { info :: {module:: Maybe String }
  , package :: String
  }

find_package :: FetchImpl -> String -> Aff (Maybe String)
find_package f name = do
  json_ <- json =<< fetch f (URL ("https://pursuit.purescript.org/search?q=" <> name))
    { method: getMethod
    , headers: makeHeaders { "Accept": "application/json" }
    }
  pure case read json_:: E (Array Item) of
    Left _ -> Nothing
    Right results -> results
        # Array.find (\{info: {module: module_}} -> module_ == Just name)
        <#> _.package

main :: Effect Unit
main = launchAff_ do
    package <- find_package nodeFetch "Prelude"
    logShow package