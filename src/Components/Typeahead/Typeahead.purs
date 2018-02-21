module CN.UI.Components.Typeahead where

import Prelude

import Data.Fuzzy (Fuzzy)
import Data.Maybe (Maybe(Nothing, Just))
import Data.StrMap (StrMap)
import Control.Monad.Aff.Class (class MonadAff)
import Data.Time.Duration (Milliseconds(..))
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(NotAsked))
import Select.Primitives.Container as C
import Select.Primitives.Search as S

import CN.UI.Block.ItemContainer as ItemContainer

import CN.UI.Core.Typeahead as TA
import CN.UI.Block.Input as Input

----------
-- Default typeahead configurations

-- A default single-select that is provided with a renderFuzzy and renderItem function.
defaultSingle :: ∀ o item source err eff m
  . MonadAff (TA.Effects eff) m
 => Eq item
 => Show err
 => Array item
 -> (item -> StrMap String)
 -> (Fuzzy item -> HH.PlainHTML)
 -> (item -> HH.PlainHTML)
 -> TA.TypeaheadInput o item source err (TA.Effects eff) m
defaultSingle xs toStrMap renderFuzzy renderItem =
  { items: TA.Sync xs
  , search: Nothing
  , initialSelection: TA.One Nothing
  , render: renderTA renderFuzzy renderItem
  , config: defaultConfig toStrMap
  }

-- A default multi-select limited to N total possible selections.
defaultLimit :: ∀ o item source err eff m
  . MonadAff (TA.Effects eff) m
 => Eq item
 => Show err
 => Int
 -> Array item
 -> (item -> StrMap String)
 -> (Fuzzy item -> HH.PlainHTML)
 -> (item -> HH.PlainHTML)
 -> TA.TypeaheadInput o item source err (TA.Effects eff) m
defaultLimit n xs toStrMap renderFuzzy renderItem =
  { items: TA.Sync xs
  , search: Nothing
  , initialSelection: TA.Limit n []
  , render: renderTA renderFuzzy renderItem
  , config: defaultConfig toStrMap
  }

-- A default multi-select that is provided with a renderFuzzy and renderItem function to determine
-- rendering a specific item in the container
defaultMulti :: ∀ o item source err eff m
  . MonadAff (TA.Effects eff) m
 => Eq item
 => Show err
 => Array item
 -> (item -> StrMap String)
 -> (Fuzzy item -> HH.PlainHTML)
 -> (item -> HH.PlainHTML)
 -> TA.TypeaheadInput o item source err (TA.Effects eff) m
defaultMulti xs toStrMap renderFuzzy renderItem =
  { items: TA.Sync xs
  , search: Nothing
  , initialSelection: TA.Many []
  , render: renderTA renderFuzzy renderItem
  , config: defaultConfig toStrMap
  }

-- A default async single select using the default render function
defaultAsyncSingle :: ∀ o item source err eff m
  . MonadAff (TA.Effects eff) m
  => Eq item
  => Show err
  => source
  -> (item -> StrMap String)
  -> (Fuzzy item -> HH.PlainHTML)
  -> (item -> HH.PlainHTML)
  -> TA.TypeaheadInput o item source err (TA.Effects eff) m
defaultAsyncSingle source toStrMap renderFuzzy renderItem =
  { items: TA.Async source NotAsked
  , search: Nothing
  , initialSelection: TA.One Nothing
  , render: renderTA renderFuzzy renderItem
  , config: defaultConfig toStrMap
  }

-- A default multi-select using the default render item function
defaultAsyncMulti :: ∀ o item source err eff m
  . MonadAff (TA.Effects eff) m
 => Eq item
 => Show err
 => source
 -> (item -> StrMap String)
 -> (Fuzzy item -> HH.PlainHTML)
 -> (item -> HH.PlainHTML)
 -> TA.TypeaheadInput o item source err (TA.Effects eff) m
defaultAsyncMulti source toStrMap renderFuzzy renderItem =
  { items: TA.Async source NotAsked
  , search: Nothing
  , initialSelection: TA.Many []
  , render: renderTA renderFuzzy renderItem
  , config: defaultConfig toStrMap
  }

-- A continuous asynchronous typeahead, reasonably debounced and
-- not filtered.
defaultContAsyncMulti :: ∀ o item source err eff m
  . MonadAff (TA.Effects eff) m
 => Eq item
 => Show err
 => source
 -> (item -> StrMap String)
 -> (Fuzzy item -> HH.PlainHTML)
 -> (item -> HH.PlainHTML)
 -> TA.TypeaheadInput o item source err (TA.Effects eff) m
defaultContAsyncMulti source toStrMap renderFuzzy renderItem =
  { items: TA.ContinuousAsync (Milliseconds 500.0) "" source NotAsked
  , search: Nothing
  , initialSelection: TA.Many []
  , render: renderTA renderFuzzy renderItem
  , config: contAsyncConfig toStrMap
  }


----------
-- Default Configuration

defaultConfig :: ∀ item
  . Eq item
 => (item -> StrMap String)
 -> TA.Config item
defaultConfig toStrMap =
  { insertable: TA.NotInsertable
  , filterType: TA.FuzzyMatch
  , keepOpen: true
  , toStrMap
  }

-- toStrMap should take an item and produce a string map
-- include the keys / values you want to filter against
-- https://github.com/citizennet/purescript-fuzzy/blob/develop/test/Main.purs
defaultFuzzyConfig :: ∀ item
  . Eq item
 => (item -> StrMap String)
 -> TA.Config item
defaultFuzzyConfig toStrMap =
  { insertable: TA.NotInsertable
  , filterType: TA.FuzzyMatch
  , keepOpen: true
  , toStrMap
  }

contAsyncConfig :: ∀ item
  . Eq item
 => (item -> StrMap String)
 -> TA.Config item
contAsyncConfig toStrMap =
  { insertable: TA.NotInsertable
  , filterType: TA.FuzzyMatch
  , keepOpen: true
  , toStrMap
  }


type TAParentHTML o item source err eff m
  = H.ParentHTML (TA.TypeaheadQuery o item source err eff m) (TA.ChildQuery o (Fuzzy item) eff) TA.ChildSlot m

----------
-- Render functions

renderTA :: ∀ o item source err eff m
  . MonadAff (TA.Effects eff) m
 => Eq item
 => (Fuzzy item -> HH.PlainHTML)
 -> (item -> HH.PlainHTML)
 -> TA.TypeaheadState item source err
 -> TAParentHTML o item source err (TA.Effects eff) m
renderTA renderFuzzy renderSelection st =
  HH.div
  [ HP.class_ $ HH.ClassName "w-full px-3" ]
  [ renderSelections
  , HH.slot'
      CP.cp2
      TA.SearchSlot
      S.component
      { render: renderSearch
      , search: Nothing
      , debounceTime: case st.items of
          (TA.ContinuousAsync db _ _ _) -> db
          _ -> Milliseconds 0.0
      }
      (HE.input TA.HandleSearch)
  , HH.slot'
      CP.cp1
      TA.ContainerSlot
      C.component
      { render: renderContainer, items: [] }
      (HE.input TA.HandleContainer)
  ]
  where
    renderSelections =
      case st.selections of
        (TA.One Nothing) -> HH.div_ []
        (TA.One (Just x)) ->  HH.div_ [ ItemContainer.selectionContainer [ ItemContainer.selectionGroup renderSelection (props x) x ] ]
        (TA.Many xs) -> HH.div_ [ ItemContainer.selectionContainer $ (\x -> ItemContainer.selectionGroup renderSelection (props x) x) <$> xs ]
        (TA.Limit _ xs) -> HH.div_ [ ItemContainer.selectionContainer $ (\x -> ItemContainer.selectionGroup renderSelection (props x) x) <$> xs ]
			where
				props item = [ HE.onClick (HE.input_ (TA.Remove item)) ]

    renderContainer cst = HH.div [ HP.class_ $ HH.ClassName "relative" ]
      if not cst.open
        then []
        else [ ItemContainer.itemContainer cst.highlightedIndex (renderFuzzy <$> cst.items) ]

    renderSearch sst =
      HH.div_
        [ Input.input
          ( S.getInputProps
            [ HP.placeholder "Type to search..."
            , HP.value sst.search
            ]
          )
        ]

