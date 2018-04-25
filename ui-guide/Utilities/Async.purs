module UIGuide.Utilities.Async where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Timer (setTimeout, TIMER)
import Data.Argonaut (Json, decodeJson, (.?))
import Data.Array (head, last)
import Data.Either (Either)
import Data.Fuzzy (Fuzzy(..))
import Data.Maybe (fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.StrMap (StrMap, fromFoldable)
import Data.String (Pattern(..), split)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax (get, AJAX)
import Network.RemoteData (RemoteData, fromEither)
import Ocelot.Block.ItemContainer as ItemContainer
import Ocelot.Components.Typeahead.Input as TA


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

renderItemUser :: ∀ o eff. TA.RenderTypeaheadItem o User eff
renderItemUser =
  { toStrMap: userToStrMap
  , renderItem: renderUser
  , renderContainer: TA.defRenderContainer' renderFuzzyUser
  }

userToStrMap :: User -> StrMap String
userToStrMap (User { name, eyeColor, hairColor, skinColor }) =
  fromFoldable
    [ Tuple "name" name
    , Tuple "eyeColor" eyeColor
    , Tuple "hairColor" hairColor
    , Tuple "skinColor" skinColor
    ]

renderUser :: ∀ p i. User -> HH.HTML p i
renderUser u@(User { name }) =
  HH.div
    [ HP.classes $ HH.ClassName <$> [ "flex", "items-center" ] ]
    [ renderUserImg u
    , HH.text name
    ]

renderFuzzyUser :: ∀ p i. Fuzzy User -> HH.HTML p i
renderFuzzyUser f@(Fuzzy { original: u }) =
  HH.div
    [ HP.classes $ HH.ClassName <$> [ "flex", "items-center" ] ]
    [ renderUserImg u
    , HH.span_ $ ItemContainer.boldMatches "name" f
    ]

renderUserImg :: ∀ p i. User -> HH.HTML p i
renderUserImg (User { eyeColor, hairColor, skinColor }) =
  let skinColors = split (Pattern ", ") skinColor
      skinColor1 = fromMaybe "" $ head skinColors
      skinColor2 = fromMaybe skinColor1 $ last skinColors in
  HH.span
    [ HP.classes $ HH.ClassName <$>
      [ "inline-block"
      , "mr-3"
      , "w-6"
      , "h-6"
      , "rounded-full"
      , "overflow-hidden"
      , "border-t-4"
      , "border-l-2"
      , "border-r-2"
      , "border-" <> colorToCSSColor hairColor
      , "bg-" <> colorToCSSColor skinColor1
      , "relative"
      , "shadow"
      ]
    ]
    [ HH.span
      [ HP.classes $ HH.ClassName <$>
        [ "flex"
        , "justify-around"
        , "pt-1"
        ]
      ]
      ( ( \_ ->
          HH.span
            [ HP.classes $ HH.ClassName <$>
              [ "w-1"
              , "h-1"
              , "rounded-full"
              , "bg-" <> colorToCSSColor eyeColor
              , "shadow-inner"
              ]
            ]
            []
        ) <$> [ 1, 2 ]
      )
    , HH.span
      [ HP.classes $ HH.ClassName <$>
        [ "pin-b"
        , "h-1"
        , "w-full"
        , "bg-" <> colorToCSSColor skinColor2
        , "absolute"
        ]
      ]
      []
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

renderItemLocation :: ∀ o eff. TA.RenderTypeaheadItem o Location eff
renderItemLocation =
  { toStrMap: locationToStrMap
  , renderItem: TA.defRenderItem <<< unwrap
  , renderContainer: TA.defRenderContainer' TA.defRenderFuzzy
  }

----------
-- Helper for rendering user img

colorToCSSColor :: String -> String
colorToCSSColor color =
  case color of
    "blond" -> "yellow-dark"
    "blonde" -> "yellow-dark"
    "fair" -> "orange-lighter"
    "blue" -> "blue-light"
    "gold" -> "yellow-dark"
    "yellow" -> "yellow"
    "white" -> "grey-lighter"
    "red" -> "red-dark"
    "brown" -> "orange-darker"
    "light" -> "orange-lighter"
    "grey" -> "grey"
    "black" -> "black"
    "auburn" -> "orange-darkest"
    "blue-gray" -> "indigo-light"
    "green" -> "green-dark"
    "green-tan" -> "green-lighter"
    "orange" -> "orange"
    "hazel" -> "green-dark"
    "pale" -> "orange-lightest"
    "metal" -> "grey"
    "dark" -> "yellow-darkest"
    "pink" -> "pink-lighter"
    "tan" -> "orange-dark"
    "silver" -> "grey-lighter"
    "brown mottle" -> "orange-darker"
    "mottled green" -> "green-dark"
    otherwise -> "transparent"
