module UIGuide.Utilities.Async where

import Prelude

import Ocelot.Block.ItemContainer as ItemContainer
import Ocelot.Components.Typeahead as TA
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


----------
-- Async types

newtype Source item = Source
  { path :: String
  , speed :: Speed
  , decoder :: Json -> RemoteData Err (Array item)
  }

data Speed
  = Fast
  | Slow
  | Fail

type Err = String

----------
-- Sources

users :: Source User
users = Source
  { path: "https://swapi.co/api/people/?search="
  , speed: Fast
  , decoder: decodeWith decodeUser
  }

locations :: Source Location
locations = Source
  { path: "https://swapi.co/api/planets/?search="
  , speed: Fast
  , decoder: decodeWith decodeLocation
  }

slow :: Source User
slow = users

fail :: Source User
fail = users


----------
-- Functions

-- Given a source, load the resulting data.
loadFromSource
  :: ∀ eff item
   . Source item
  -> String
  -> Aff (ajax :: AJAX, timer :: TIMER | eff) (RemoteData Err (Array item))
loadFromSource (Source { path, speed, decoder }) search =
  case speed of
    Fast -> get (path <> search) >>= (pure <<< decoder <<< _.response)
    Fail -> get search >>= (pure <<< decoder <<< _.response)
    Slow -> do
      _ <- liftEff $ setTimeout 5000 (pure unit)
      res <- get (path <> search)
      pure $ decoder res.response

----------
-- Types for the JSON API

decodeWith
  :: ∀ item
   . (Json -> Either String item)
  -> Json
  -> RemoteData Err (Array item)
decodeWith decoder json =
  fromEither $ traverse decoder =<< decodeResults =<< decodeJson json

decodeResults :: Json -> Either String (Array Json)
decodeResults json = do
  obj <- decodeJson json
  resultsJson <- obj .? "results"
  results <- decodeJson resultsJson
  pure $ results

newtype User = User
  { name :: String
  , eyeColor :: String
  , hairColor :: String
  , skinColor :: String
  }

derive instance newtypeUser :: Newtype User _
derive instance eqUser :: Eq User
instance showUser :: Show User where
  show (User { name }) = name

decodeUser :: Json -> Either String User
decodeUser json = do
  obj <- decodeJson json
  name <- obj .? "name"
  eyeColor <- obj .? "eye_color"
  hairColor <- obj .? "hair_color"
  skinColor <- obj .? "skin_color"
  pure $ User
    { name
    , eyeColor
    , hairColor
    , skinColor
    }

renderItemUser :: TA.RenderTypeaheadItem User
renderItemUser =
  { toStrMap: userToStrMap
  , renderItem: TA.defRenderItem <<< unwrap
  , renderFuzzy: TA.defRenderFuzzy
  }

userToStrMap :: User -> StrMap String
userToStrMap (User { name, eyeColor, hairColor, skinColor }) =
  fromFoldable
    [ Tuple "name" name
    , Tuple "eyeColor" eyeColor
    , Tuple "hairColor" hairColor
    , Tuple "skinColor" skinColor
    ]

newtype Location = Location
  { name :: String
  , population :: String
  }

derive instance newtypeLocation :: Newtype Location _
derive instance eqLocation :: Eq Location
instance showLocation :: Show Location where
  show (Location { name, population }) =
    name <> " (" <> show population <> " population)"

decodeLocation :: Json -> Either String Location
decodeLocation json = do
  obj <- decodeJson json
  name <- obj .? "name"
  population <- obj .? "population"
  pure $ Location { name, population }

locationToStrMap :: Location -> StrMap String
locationToStrMap (Location { name, population }) =
  fromFoldable [ Tuple "name" name ]

renderItemLocation :: TA.RenderTypeaheadItem Location
renderItemLocation =
  { toStrMap: locationToStrMap
  , renderItem: TA.defRenderItem <<< unwrap
  , renderFuzzy: TA.defRenderFuzzy
  }


