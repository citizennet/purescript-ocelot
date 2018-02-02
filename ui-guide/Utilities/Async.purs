module UIGuide.Utilities.Async where

import Prelude

import Control.Monad.Eff.Timer (setTimeout, TIMER)
import Data.Maybe (Maybe(..))
import Data.Either (Either)
import Data.Argonaut (Json, decodeJson, (.?))
import Network.HTTP.Affjax (get, AJAX)
import Network.RemoteData (RemoteData, fromEither)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import CN.UI.Core.Typeahead (class StringComparable, SyncMethod(..), toString)

import Data.Traversable (traverse)


----------
-- Async types

data Item
  = Users User
  | Todos Todo

derive instance eqItem :: Eq Item

instance showItem :: Show Item where
  show (Users u) = show u
  show (Todos t) = show t

instance stringComparableItem :: StringComparable Item where
  toString (Users u) = toString u
  toString (Todos t) = toString t

type Source item =
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

users :: Source Item
users =
  { path: "users"
  , root: "https://jsonplaceholder.typicode.com/"
  , speed: Fast
  , decoder: (map <<< map <<< map) (\x -> Users x) $ decodeWith decodeUser
  }

todos :: Source Item
todos =
  { path: "todos"
  , root: "https://jsonplaceholder.typicode.com/"
  , speed: Fast
  , decoder: (map <<< map <<< map) (\x -> Todos x) $ decodeWith decodeTodo
  }

slow :: Source Item
slow = users { speed = Slow }

fail :: Source Item
fail = users { speed = Fail }


----------
-- Functions

-- Not yet using 'search'
-- load :: ∀ eff item. SyncMethod (Source item) Err (Array item) -> Aff ( ajax :: AJAX, timer :: TIMER | eff ) (Maybe (RemoteData Err (Array item)))
load (Sync _) = pure Nothing
load (Async src _) = Just <$> loadFromSource src
load (ContinuousAsync search src _) = Just <$> loadFromSource src

-- Given a source, load the resulting data.
-- loadFromSource :: ∀ eff item. Source item -> Aff ( ajax :: AJAX, timer :: TIMER | eff ) (RemoteData Err (Array item))
loadFromSource { root, path, speed, decoder } = case speed of
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

instance stringComparableTodo :: StringComparable Todo where
  toString (Todo { title }) = title

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


