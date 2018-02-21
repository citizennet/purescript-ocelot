module CN.UI.Components.Typeahead where

import Prelude

import Data.Fuzzy (Fuzzy)
import Data.Maybe (Maybe(..))
import Data.StrMap (StrMap, fromFoldable, singleton)
import Data.Tuple (Tuple(..))
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
-- Input types expected. This needs to be defined for each 'item' type we have.

type RenderTypeaheadItem item
  = { toStrMap :: item -> StrMap String
    , renderFuzzy :: Fuzzy item -> HH.PlainHTML
    , renderItem :: item -> HH.PlainHTML }

renderItemString :: RenderTypeaheadItem String
renderItemString =
  { toStrMap: singleton "name"
  , renderFuzzy: defRenderFuzzy
  , renderItem: HH.text }


----------
-- Default rendering

defToStrMap :: ∀ r. { name :: String | r } -> StrMap String
defToStrMap { name } = fromFoldable [ Tuple "name" name ]

-- WARNING: This expects you to have a string map with the "name"
-- key present or else it will not work but will compile!
defRenderFuzzy :: ∀ item. Fuzzy item -> HH.PlainHTML
defRenderFuzzy = HH.span_ <<< ItemContainer.boldMatches "name"

defRenderItem :: ∀ r. { name :: String | r } -> HH.PlainHTML
defRenderItem { name } = HH.text name


----------
-- Default typeahead configurations

-- A def single-select that is provided with a renderFuzzy and renderItem function.
defSingle :: ∀ o item source err eff m
  . MonadAff (TA.Effects eff) m
 => Eq item
 => Show err
 => Array item
 -> RenderTypeaheadItem item
 -> TA.TypeaheadInput o item source err (TA.Effects eff) m
defSingle xs { toStrMap, renderFuzzy, renderItem } =
  { items: TA.Sync xs
  , search: Nothing
  , initialSelection: TA.One Nothing
  , render: renderTA renderFuzzy renderItem
  , config: defConfig toStrMap
  }

-- A def multi-select limited to N total possible selections.
defLimit :: ∀ o item source err eff m
  . MonadAff (TA.Effects eff) m
 => Eq item
 => Show err
 => Int
 -> Array item
 -> RenderTypeaheadItem item
 -> TA.TypeaheadInput o item source err (TA.Effects eff) m
defLimit n xs { toStrMap, renderFuzzy, renderItem } =
  { items: TA.Sync xs
  , search: Nothing
  , initialSelection: TA.Limit n []
  , render: renderTA renderFuzzy renderItem
  , config: defConfig toStrMap
  }

-- A def multi-select that is provided with a renderFuzzy and renderItem function to determine
-- rendering a specific item in the container
defMulti :: ∀ o item source err eff m
  . MonadAff (TA.Effects eff) m
 => Eq item
 => Show err
 => Array item
 -> RenderTypeaheadItem item
 -> TA.TypeaheadInput o item source err (TA.Effects eff) m
defMulti xs { toStrMap, renderFuzzy, renderItem } =
  { items: TA.Sync xs
  , search: Nothing
  , initialSelection: TA.Many []
  , render: renderTA renderFuzzy renderItem
  , config: defConfig toStrMap
  }

-- A def async single select using the default render function
defAsyncSingle :: ∀ o item source err eff m
  . MonadAff (TA.Effects eff) m
  => Eq item
  => Show err
  => source
  -> RenderTypeaheadItem item
  -> TA.TypeaheadInput o item source err (TA.Effects eff) m
defAsyncSingle source { toStrMap, renderFuzzy, renderItem } =
  { items: TA.Async source NotAsked
  , search: Nothing
  , initialSelection: TA.One Nothing
  , render: renderTA renderFuzzy renderItem
  , config: defConfig toStrMap
  }

-- A def multi-select using the default render item function
defAsyncMulti :: ∀ o item source err eff m
  . MonadAff (TA.Effects eff) m
 => Eq item
 => Show err
 => source
 -> RenderTypeaheadItem item
 -> TA.TypeaheadInput o item source err (TA.Effects eff) m
defAsyncMulti source { toStrMap, renderFuzzy, renderItem } =
  { items: TA.Async source NotAsked
  , search: Nothing
  , initialSelection: TA.Many []
  , render: renderTA renderFuzzy renderItem
  , config: defConfig toStrMap
  }

-- A continuous asynchronous typeahead, reasonably debounced and
-- not filtered.
defContAsyncMulti :: ∀ o item source err eff m
  . MonadAff (TA.Effects eff) m
 => Eq item
 => Show err
 => source
 -> RenderTypeaheadItem item
 -> TA.TypeaheadInput o item source err (TA.Effects eff) m
defContAsyncMulti source { toStrMap, renderFuzzy, renderItem } =
  { items: TA.ContinuousAsync (Milliseconds 500.0) "" source NotAsked
  , search: Nothing
  , initialSelection: TA.Many []
  , render: renderTA renderFuzzy renderItem
  , config: contAsyncConfig toStrMap
  }


----------
-- Default Configuration

defConfig :: ∀ item
  . Eq item
 => (item -> StrMap String)
 -> TA.Config item
defConfig toStrMap =
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


----------
-- Render function

type TAParentHTML o item source err eff m
  = H.ParentHTML (TA.TypeaheadQuery o item source err eff m) (TA.ChildQuery o (Fuzzy item) eff) TA.ChildSlot m

renderTA :: ∀ o item source err eff m
  . MonadAff (TA.Effects eff) m
 => Eq item
 => (Fuzzy item -> HH.PlainHTML)
 -> (item -> HH.PlainHTML)
 -> TA.TypeaheadState item source err
 -> TAParentHTML o item source err (TA.Effects eff) m
renderTA renderFuzzy renderSelection st =
  HH.div_
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

    renderSearch sst = HH.div_
      [ Input.input
        ( S.getInputProps
          [ HP.placeholder "Type to search...", HP.value sst.search ]
        )
      ]
