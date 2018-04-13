module UIGuide.Utilities.Async where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Timer (setTimeout, TIMER)
import Data.Argonaut (Json, decodeJson, (.?))
import Data.Either (Either)
import Data.Newtype (class Newtype, unwrap)
import Data.StrMap (StrMap, fromFoldable)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Halogen.HTML as HH
import Network.HTTP.Affjax (get, AJAX)
import Network.RemoteData (RemoteData, fromEither)
import Ocelot.Block.ItemContainer as ItemContainer
import Ocelot.Components.Typeahead as TA


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
    , Tuple "completed" (if completed then "Completed" else "")
    ]

renderItemTodo :: ∀ o eff. TA.RenderTypeaheadItem o Todo eff
renderItemTodo =
  { toStrMap: todoToStrMap
  , renderItem: HH.text <<< _.title <<< unwrap
  , renderContainer: TA.defRenderContainer_ (HH.span_ <<< ItemContainer.boldMatches "title")
  }


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

renderItemUser :: ∀ o eff. TA.RenderTypeaheadItem o User eff
renderItemUser =
  { toStrMap: userToStrMap
  , renderItem: TA.defRenderItem <<< unwrap
  , renderContainer: TA.defRenderContainer_ TA.defRenderFuzzy
  }

userToStrMap :: User -> StrMap String
userToStrMap (User { id, name, city }) =
  fromFoldable
    [ Tuple "id" (show id)
    , Tuple "name" name
    , Tuple "city" city
    ]
