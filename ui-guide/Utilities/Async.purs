module UIGuide.Utilities.Async where

import Prelude

import Control.Monad.Aff (Aff)
import Network.HTTP.Affjax (get, AJAX)
import Control.Monad.Eff.Timer (setTimeout, TIMER)

import Data.Maybe (Maybe(..))
import Data.Either (Either)
import Data.Argonaut (Json, decodeJson, (.?))
import Network.RemoteData (RemoteData, fromEither)
import Control.Monad.Eff.Class (liftEff)

import CN.UI.Core.Typeahead (class CompareToString, SyncMethod(..), compareToString)

import Data.Traversable (traverse)


----------
-- Async types

newtype Source item = Source
  { path :: String
  , root :: String
  , speed :: Speed
  , decoder :: Json -> RemoteData Err (Array item) }

data Speed
  = Fast
  | Slow
  | Fail

type Err = String

----------
-- Sources

users :: Source User
users = Source
  { path: "users"
  , root: "https://jsonplaceholder.typicode.com/"
  , speed: Fast
  , decoder: decodeWith decodeUser
  }

todos :: Source Todo
todos = Source
  { path: "todos"
  , root: "https://jsonplaceholder.typicode.com/"
  , speed: Fast
  , decoder: decodeWith decodeTodo
  }

slow :: Source User
slow = users

fail :: Source User
fail = users


----------
-- Functions

-- Not yet using 'search'
load :: ∀ eff item
  . SyncMethod (Source item) Err (Array item)
 -> Aff (ajax :: AJAX, timer :: TIMER | eff) (Maybe (RemoteData Err (Array item)))
load (Sync _) = pure Nothing
load (Async src _) = Just <$> loadFromSource src
load (ContinuousAsync _ search src _) = Just <$> loadFromSource src

-- Given a source, load the resulting data.
loadFromSource :: ∀ eff item
  . Source item
 -> Aff (ajax :: AJAX, timer :: TIMER | eff) (RemoteData Err (Array item))
loadFromSource (Source { root, path, speed, decoder }) = case speed of
  Fast -> get (root <> path) >>= (pure <<< decoder <<< _.response)
  Fail -> get path >>= (pure <<< decoder <<< _.response)
  Slow -> do
    _ <- liftEff $ setTimeout 5000 (pure unit)
    res <- get (root <> path)
    pure $ decoder res.response


----------
-- Typeas for the JSON API

decodeWith :: ∀ item. (Json -> Either String item) -> Json -> RemoteData Err (Array item)
decodeWith decoder json = fromEither $ traverse decoder =<< decodeJson json

newtype Todo = Todo
  { title :: String
  , completed :: Boolean }

derive instance eqTodo :: Eq Todo

instance showTodo :: Show Todo where
  show (Todo { title, completed }) = "Todo: " <> title <> " " <> show completed

instance stringComparableTodo :: CompareToString Todo where
  compareToString (Todo { title }) = title

decodeTodo :: Json -> Either String Todo
decodeTodo json = do
  obj <- decodeJson json
  title <- obj .? "title"
  completed <- obj .? "completed"
  pure $ Todo { title, completed }



newtype User = User
  { id :: Int
  , name :: String
  , city :: String }

derive instance eqUser :: Eq User
instance showUser :: Show User where
  show (User { id, name }) = show id <> ": " <> name
instance stringComparableUser :: CompareToString User where
  compareToString (User { name }) = name

decodeUser :: Json -> Either String User
decodeUser json = do
  obj <- decodeJson json

  name <- obj .? "name"
  id <- obj .? "id"
  city <- obj .? "address" >>= \i -> i .? "city"

  pure $ User { id, name, city }


