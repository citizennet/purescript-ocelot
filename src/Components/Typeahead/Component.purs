-- | The typeahead module intended for imports
module Ocelot.Component.Typeahead
  ( module Ocelot.Component.Typeahead.Base
  , module TA
  , DefaultSyncTypeaheadInput(..)
  , DefaultAsyncTypeaheadInput(..)
  , syncSingle
  , syncMulti
  , asyncSingle
  , asyncMulti
  ) where

import Ocelot.Component.Typeahead.Base

import Prelude (class Eq, (<<<), ($))
import DOM.HTML.Indexed (HTMLinput)
import Data.Fuzzy (Fuzzy, match)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff.Class (class MonadAff)
import Foreign.Object (Object)
import Halogen.HTML as HH
import Network.RemoteData (RemoteData(..))
import Ocelot.Component.Typeahead.Render (defRenderContainer, isDisabled, renderMulti, renderSingle) as TA

-- | Forgive the long name; it provides clarity into what exactly
-- | this type represents and you don't ordinarily need to write
-- | it out.
type DefaultSyncTypeaheadInput item =
  { itemToObject :: item -> Object String
  , renderFuzzy :: Fuzzy item -> HH.PlainHTML
  }

syncSingle
  :: ∀ action item m
   . Eq item
  => MonadAff m
  => DefaultSyncTypeaheadInput item
  -> Array (HH.IProp HTMLinput (CompositeAction action Maybe item m))
  -> Input action Maybe item m
syncSingle { itemToObject, renderFuzzy } props =
  { items: NotAsked
  , insertable: NotInsertable
  , keepOpen: false
  , itemToObject
  , debounceTime: Nothing
  , async: Nothing
  , disabled: TA.isDisabled props
  , render: TA.renderSingle
      props
      (renderFuzzy <<< match false itemToObject "")
      (TA.defRenderContainer renderFuzzy)
  }

syncMulti
  :: ∀ action item m
   . Eq item
  => MonadAff m
  => DefaultSyncTypeaheadInput item
  -> Array (HH.IProp HTMLinput (CompositeAction action Array item m))
  -> Input action Array item m
syncMulti { itemToObject, renderFuzzy } props =
  { items: NotAsked
  , insertable: NotInsertable
  , keepOpen: true
  , itemToObject
  , debounceTime: Nothing
  , async: Nothing
  , disabled: TA.isDisabled props
  , render: TA.renderMulti
      props
      (renderFuzzy <<< match false itemToObject "")
      (TA.defRenderContainer renderFuzzy)
  }

-- | Forgive the long name; it provides clarity into what exactly
-- | this type represents and you don't ordinarily need to write
-- | it out.
type DefaultAsyncTypeaheadInput item m =
  { itemToObject :: item -> Object String
  , renderFuzzy :: Fuzzy item -> HH.PlainHTML
  , async :: String -> m (RemoteData String (Array item))
  }

asyncSingle
  :: ∀ action item m
   . Eq item
  => MonadAff m
  => DefaultAsyncTypeaheadInput item m
  -> Array (HH.IProp HTMLinput (CompositeAction action Maybe item m))
  -> Input action Maybe item m
asyncSingle { async, itemToObject, renderFuzzy } props =
  { items: NotAsked
  , insertable: NotInsertable
  , keepOpen: false
  , itemToObject
  , debounceTime: Just $ Milliseconds 300.0
  , async: Just async
  , disabled: TA.isDisabled props
  , render: TA.renderSingle
      props
      (renderFuzzy <<< match false itemToObject "")
      (TA.defRenderContainer renderFuzzy)
  }

asyncMulti
  :: ∀ action item m
   . Eq item
  => MonadAff m
  => DefaultAsyncTypeaheadInput item m
  -> Array (HH.IProp HTMLinput (CompositeAction action Array item m))
  -> Input action Array item m
asyncMulti { async, itemToObject, renderFuzzy } props =
  { items: NotAsked
  , insertable: NotInsertable
  , keepOpen: true
  , itemToObject
  , debounceTime: Just $ Milliseconds 300.0
  , async: Just async
  , disabled: TA.isDisabled props
  , render: TA.renderMulti
      props
      (renderFuzzy <<< match false itemToObject "")
      (TA.defRenderContainer renderFuzzy)
  }
