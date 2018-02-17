module UIGuide.Utilities.Async where

import Prelude

import CN.UI.Components.Typeahead as TA
import CN.UI.Core.Typeahead (SyncMethod(..))
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Timer (setTimeout, TIMER)
import Data.Argonaut (Json, decodeJson, (.?))
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.StrMap (StrMap, fromFoldable)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Halogen.HTML as HH
import Network.HTTP.Affjax (get, AJAX)
import Network.RemoteData (RemoteData(..), fromEither)


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
 -> Aff (ajax :: AJAX, timer :: TIMER | eff) (RemoteData Err (Array item))
load (Sync xs) = pure $ Success xs
load (Async src _) = loadFromSource src
load (ContinuousAsync _ search src _) = loadFromSource src

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
-- Types for the JSON API

decodeWith :: ∀ item. (Json -> Either String item) -> Json -> RemoteData Err (Array item)
decodeWith decoder json = fromEither $ traverse decoder =<< decodeJson json

newtype Todo = Todo
  { title :: String
  , completed :: Boolean }

derive instance newtypeTodo :: Newtype Todo _
derive instance eqTodo :: Eq Todo

instance showTodo :: Show Todo where
  show (Todo { title, completed }) = "Todo: " <> title <> " " <> show completed

decodeTodo :: Json -> Either String Todo
decodeTodo json = do
  obj <- decodeJson json
  title <- obj .? "title"
  completed <- obj .? "completed"
  pure $ Todo { title, completed }

todoToStrMap :: Todo -> StrMap String
todoToStrMap (Todo { title, completed }) =
  fromFoldable
    [ Tuple "title" title
    , Tuple "completed" (if completed then "completed" else "")
    ]

todoRenderFuzzy :: ∀ o. TA.RenderContainerRow o Todo
todoRenderFuzzy = TA.defaultContainerRow $ TA.boldMatches "title"

todoRenderItem :: ∀ o source err eff m
  . Todo -> TA.TAParentHTML o Todo source err eff m
todoRenderItem = TA.defaultSelectionRow $ HH.text <<< _.title <<< unwrap

newtype User = User
  { id :: Int
  , name :: String
  , city :: String }

derive instance newtypeUser :: Newtype User _
derive instance eqUser :: Eq User
instance showUser :: Show User where
  show (User { id, name }) = show id <> ": " <> name

decodeUser :: Json -> Either String User
decodeUser json = do
  obj <- decodeJson json

  name <- obj .? "name"
  id <- obj .? "id"
  city <- obj .? "address" >>= \i -> i .? "city"

  pure $ User { id, name, city }

userToStrMap :: User -> StrMap String
userToStrMap (User { id, name, city }) =
  fromFoldable
    [ Tuple "id" (show id)
    , Tuple "name" name
    , Tuple "city" city
    ]

userRenderFuzzy :: ∀ o. TA.RenderContainerRow o User
userRenderFuzzy = TA.defaultContainerRow $ TA.boldMatches "name"

userRenderItem :: ∀ o source err eff m
  . User -> TA.TAParentHTML o User source err eff m
userRenderItem = TA.defaultSelectionRow $ HH.text <<< _.name <<< unwrap
