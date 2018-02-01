module UIGuide.Utilities.Async where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Either (Either)
import Data.Argonaut (Json, decodeJson, (.?))
import Network.HTTP.Affjax (get, AJAX)
import Network.RemoteData (RemoteData, fromEither)
import Control.Monad.Aff (Aff)
import CN.UI.Core.Typeahead (class StringComparable, SyncMethod(..))

import Data.Traversable (traverse)


----------
-- Async types

type Source item =
  { path :: String
  , root :: String
  , decoder :: Json -> RemoteData Err (Array item) }

type Err = String

----------
-- Sources

users :: Source User
users =
  { path: "users"
  , root: "https://jsonplaceholder.typicode.com/"
  , decoder: decodeWith decodeUser }

todos :: Source Todo
todos =
  { path: "todos"
  , root: "https://jsonplaceholder.typicode.com/"
  , decoder: decodeWith decodeTodo }


----------
-- Functions

-- Not yet using 'search'
load :: ∀ eff item. SyncMethod (Source item) Err (Array item) -> Aff ( ajax :: AJAX | eff ) (Maybe (RemoteData Err (Array item)))
load (Sync _) = pure Nothing
load (Async src _) = Just <$> loadFromSource src
load (ContinuousAsync search src _) = Just <$> loadFromSource src

-- Given a source, load the resulting data.
loadFromSource :: ∀ eff item. Source item -> Aff ( ajax :: AJAX | eff ) (RemoteData Err (Array item))
loadFromSource { root, path, decoder } = get (root <> path) >>= (pure <<< decoder <<< _.response)

----------
-- Typeas for the JSON API

decodeWith :: ∀ item. (Json -> Either String item) -> Json -> RemoteData Err (Array item)
decodeWith decoder json = fromEither $ traverse decoder =<< decodeJson json

newtype Todo = Todo
  { title :: String
  , completed :: Boolean }

derive instance eqTodo :: Eq Todo
instance showTodo :: Show Todo where
  show (Todo { title, completed }) = title <> " " <> show completed
instance stringComparableTodo :: StringComparable Todo where
  toString (Todo { title, completed }) = title <> " " <> show completed

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
instance stringComparableUser :: StringComparable User where
  toString (User { name }) = name

decodeUser :: Json -> Either String User
decodeUser json = do
  obj <- decodeJson json

  name <- obj .? "name"
  id <- obj .? "id"
  city <- obj .? "address" >>= \i -> i .? "city"

  pure $ User { id, name, city }
