module PursPurs.Load where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(Just, Nothing))
import Data.String.Pattern (Pattern(Pattern))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class.Console (log, logShow)
import Milkis (Response, URL(URL), defaultFetchOptions, fetch, getMethod, json, makeHeaders, text)
import Milkis.Impl (FetchImpl)
import Milkis.Impl.Node (nodeFetch)
import Yoga.JSON (E, read, readJSON_)
import Data.Array (find, last) as Array
import Data.String.CodeUnits (drop, dropRight, take) as Data.String
import Data.String.Common (split) as Data.String

type Item =
  { info :: { module :: Maybe String }
  , package :: String
  }

find_package :: FetchImpl -> String -> Aff (Maybe String)
find_package f name = do
  json_ <- json =<< fetch f (URL ("https://pursuit.purescript.org/search?q=" <> name))
    { method: getMethod
    , headers: makeHeaders { "Accept": "application/json" }
    }
  pure case read json_ :: E (Array Item) of
    Left _ -> Nothing
    Right results -> results
      # Array.find (\{ info: { module: module_ } } -> module_ == Just name)
      <#> _.package

find_github_url :: FetchImpl -> String -> Aff (Maybe String)
find_github_url f name = do
  let
    prefix1 = Data.String.take 2 name
    prefix2 = Data.String.take 2 (Data.String.drop 2 name)
    url :: String
    url = "https://raw.githubusercontent.com/purescript/registry-index/main/"
      <> prefix1
      <> "/"
      <> prefix2
      <> "/"
      <> name
  (response :: Response) <- fetch f (URL url) defaultFetchOptions
  text_ <- text response


  let
    lines = Data.String.split (Pattern "\n") (Data.String.dropRight 1 text_)
    last_line = Array.last lines

    json_ :: Maybe { version :: String, location :: Location }
    json_ = do
      str :: String <- last_line
      readJSON_ str

    url_ :: Maybe String
    url_ = json_
      <#> \{ version, location: { githubOwner, githubRepo } } ->
        "https://raw.githubusercontent.com/"
          <> githubOwner
          <> "/"
          <> githubRepo
          <> "/v"
          <> version
          <> "/src/"
  pure url_

type Location = { githubOwner :: String, githubRepo :: String }

index_name :: String -> String
index_name name =
  if Data.String.take 11 name == "purescript-" then Data.String.drop 11 name
  else name

main :: Effect Unit
main = launchAff_ do
  let
    module_name = "Prelude"
  package <- find_package nodeFetch module_name
  case package of
    Nothing -> log ("could not find package for module_name: " <> module_name)
    Just package_ -> do
      url_ <- find_github_url nodeFetch (index_name package_)
      logShow url_