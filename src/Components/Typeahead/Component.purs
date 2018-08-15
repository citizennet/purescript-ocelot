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

import Prelude (class Eq, (<<<))
import DOM.HTML.Indexed (HTMLinput)
import Data.Fuzzy (Fuzzy, match)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff.Class (class MonadAff)
import Foreign.Object (Object)
import Halogen as H
import Halogen.HTML as HH
import Network.RemoteData (RemoteData(..))
import Ocelot.Component.Typeahead.Render (defRenderContainer, renderMulti, renderSingle) as TA
import Select as Select

-- | Forgive the long name; it provides clarity into what exactly
-- | this type represents and you don't ordinarily need to write
-- | it out.
type DefaultSyncTypeaheadInput item =
  { itemToObject :: item -> Object String
  , renderFuzzy :: Fuzzy item -> HH.PlainHTML
  }

syncSingle
  :: ∀ pq item m
   . MonadAff m
  => Eq item
  => DefaultSyncTypeaheadInput item
  -> Array (H.IProp HTMLinput (Select.Query (Query pq Maybe item m) (Fuzzy item)))
  -> Input pq Maybe item m
syncSingle { itemToObject, renderFuzzy } props =
  { items: NotAsked
  , insertable: NotInsertable
  , keepOpen: false
  , itemToObject
  , asyncConfig: Nothing
  , render: TA.renderSingle
      props
      (renderFuzzy <<< match false itemToObject "")
      (TA.defRenderContainer renderFuzzy)
  }

syncMulti
  :: ∀ pq item m
   . MonadAff m
  => Eq item
  => DefaultSyncTypeaheadInput item
  -> Array (H.IProp HTMLinput (Select.Query (Query pq Array item m) (Fuzzy item)))
  -> Input pq Array item m
syncMulti { itemToObject, renderFuzzy } props =
  { items: NotAsked
  , insertable: NotInsertable
  , keepOpen: true
  , itemToObject
  , asyncConfig: Nothing
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
  , fetchItems :: String -> m (RemoteData String (Array item))
  }

asyncSingle
  :: ∀ pq item m
   . MonadAff m
  => Eq item
  => DefaultAsyncTypeaheadInput item m
  -> Array (H.IProp HTMLinput (Select.Query (Query pq Maybe item m) (Fuzzy item)))
  -> Input pq Maybe item m
asyncSingle { itemToObject, renderFuzzy, fetchItems } props =
  { items: NotAsked
  , insertable: NotInsertable
  , keepOpen: true
  , itemToObject
  , asyncConfig: Just
      { debounceTime: Milliseconds 300.0
      , fetchItems
      }
  , render: TA.renderSingle
      props
      (renderFuzzy <<< match false itemToObject "")
      (TA.defRenderContainer renderFuzzy)
  }

asyncMulti
  :: ∀ pq item m
   . MonadAff m
  => Eq item
  => DefaultAsyncTypeaheadInput item m
  -> Array (H.IProp HTMLinput (Select.Query (Query pq Array item m) (Fuzzy item)))
  -> Input pq Array item m
asyncMulti { itemToObject, renderFuzzy, fetchItems } props =
  { items: NotAsked
  , insertable: NotInsertable
  , keepOpen: true
  , itemToObject
  , asyncConfig: Just
      { debounceTime: Milliseconds 300.0
      , fetchItems
      }
  , render: TA.renderMulti
      props
      (renderFuzzy <<< match false itemToObject "")
      (TA.defRenderContainer renderFuzzy)
  }
