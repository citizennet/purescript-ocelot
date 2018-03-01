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
import DOM.HTML.Indexed (HTMLinput)
import Network.RemoteData (RemoteData(NotAsked))
import Select.Primitives.SearchContainer as SC
import Select.Primitives.Container as C
import Select.Primitives.Search as S

import CN.UI.Block.ItemContainer as ItemContainer

import CN.UI.Core.Typeahead as TA
import CN.UI.Block.Type as Type
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
 => Array (H.IProp HTMLinput (S.SearchQuery o (Fuzzy item) (TA.Effects eff)))
 -> Array item
 -> RenderTypeaheadItem item
 -> TA.TypeaheadInput o item source err (TA.Effects eff) m
defSingle props xs { toStrMap, renderFuzzy, renderItem } =
  { items: TA.Sync xs
  , search: Nothing
  , initialSelection: TA.One Nothing
  , render: renderTA props renderFuzzy renderItem
  , config: defSingleConfig toStrMap
  }

-- A def multi-select limited to N total possible selections.
defLimit :: ∀ o item source err eff m
  . MonadAff (TA.Effects eff) m
 => Eq item
 => Show err
 => Array (H.IProp HTMLinput (S.SearchQuery o (Fuzzy item) (TA.Effects eff)))
 -> Int
 -> Array item
 -> RenderTypeaheadItem item
 -> TA.TypeaheadInput o item source err (TA.Effects eff) m
defLimit props n xs { toStrMap, renderFuzzy, renderItem } =
  { items: TA.Sync xs
  , search: Nothing
  , initialSelection: TA.Limit n []
  , render: renderTA props renderFuzzy renderItem
  , config: defMultiConfig toStrMap
  }

-- A def multi-select that is provided with a renderFuzzy and renderItem function to determine
-- rendering a specific item in the container
defMulti :: ∀ o item source err eff m
  . MonadAff (TA.Effects eff) m
 => Eq item
 => Show err
 => Array (H.IProp HTMLinput (S.SearchQuery o (Fuzzy item) (TA.Effects eff)))
 -> Array item
 -> RenderTypeaheadItem item
 -> TA.TypeaheadInput o item source err (TA.Effects eff) m
defMulti props xs { toStrMap, renderFuzzy, renderItem } =
  { items: TA.Sync xs
  , search: Nothing
  , initialSelection: TA.Many []
  , render: renderTA props renderFuzzy renderItem
  , config: defMultiConfig toStrMap
  }

-- A def async single select using the default render function
defAsyncSingle :: ∀ o item source err eff m
  . MonadAff (TA.Effects eff) m
  => Eq item
  => Show err
  => Array (H.IProp HTMLinput (S.SearchQuery o (Fuzzy item) (TA.Effects eff)))
  -> source
  -> RenderTypeaheadItem item
  -> TA.TypeaheadInput o item source err (TA.Effects eff) m
defAsyncSingle props source { toStrMap, renderFuzzy, renderItem } =
  { items: TA.Async source NotAsked
  , search: Nothing
  , initialSelection: TA.One Nothing
  , render: renderTA props renderFuzzy renderItem
  , config: defSingleConfig toStrMap
  }

-- A def multi-select using the default render item function
defAsyncMulti :: ∀ o item source err eff m
  . MonadAff (TA.Effects eff) m
 => Eq item
 => Show err
 => Array (H.IProp HTMLinput (S.SearchQuery o (Fuzzy item) (TA.Effects eff)))
 -> source
 -> RenderTypeaheadItem item
 -> TA.TypeaheadInput o item source err (TA.Effects eff) m
defAsyncMulti props source { toStrMap, renderFuzzy, renderItem } =
  { items: TA.Async source NotAsked
  , search: Nothing
  , initialSelection: TA.Many []
  , render: renderTA props renderFuzzy renderItem
  , config: defMultiConfig toStrMap
  }

-- A continuous asynchronous typeahead, reasonably debounced and
-- not filtered.
defContAsyncMulti :: ∀ o item source err eff m
  . MonadAff (TA.Effects eff) m
 => Eq item
 => Show err
 => Array (H.IProp HTMLinput (S.SearchQuery o (Fuzzy item) (TA.Effects eff)))
 -> source
 -> RenderTypeaheadItem item
 -> TA.TypeaheadInput o item source err (TA.Effects eff) m
defContAsyncMulti props source { toStrMap, renderFuzzy, renderItem } =
  { items: TA.ContinuousAsync (Milliseconds 500.0) "" source NotAsked
  , search: Nothing
  , initialSelection: TA.Many []
  , render: renderTA props renderFuzzy renderItem
  , config: contAsyncConfig toStrMap
  }


----------
-- Default Configuration

defSingleConfig :: ∀ item
  . Eq item
 => (item -> StrMap String)
 -> TA.Config item
defSingleConfig toStrMap =
  { insertable: TA.NotInsertable
  , filterType: TA.FuzzyMatch
  , keepOpen: false
  , toStrMap
  }

defMultiConfig :: ∀ item
  . Eq item
 => (item -> StrMap String)
 -> TA.Config item
defMultiConfig toStrMap =
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
  = H.ParentHTML (TA.TypeaheadQuery o item source err eff m) (TA.ChildQuery o (Fuzzy item) eff m) TA.ChildSlot m

renderTA :: ∀ o item source err eff m
  . MonadAff (TA.Effects eff) m
 => Eq item
 => Array (H.IProp HTMLinput (S.SearchQuery o (Fuzzy item) (TA.Effects eff)))
 -> (Fuzzy item -> HH.PlainHTML)
 -> (item -> HH.PlainHTML)
 -> TA.TypeaheadState item source err
 -> TAParentHTML o item source err (TA.Effects eff) m
renderTA props renderFuzzy renderSelectionItem st =
  renderAll $
    HH.slot
      unit
      SC.component
      searchContainerInput
      (HE.input TA.HandleSearchContainer)
  where
    searchContainerInput =
      { search: Nothing
      , debounceTime: case st.items of
          (TA.ContinuousAsync db _ _ _) -> db
          _ -> Milliseconds 0.0
      , items: []
      , renderSearch
      , renderContainer
      , render: \search container -> HH.div_ [ search, container ]
      }

    itemProps item = [ HE.onClick (HE.input_ (TA.Remove item)) ]

    renderAll slot =
      case st.selections of
        TA.One Nothing ->
          HH.div_ [ slot ]
        TA.One (Just x) ->
          HH.span_ [ renderSelection x, slot ]
        TA.Many xs ->
          HH.div_ [ renderSelections xs, slot ]
        TA.Limit _ xs ->
          HH.div_ [ renderSelections xs, slot ]

    renderSelection x =
      ItemContainer.selectionContainer
        [ ItemContainer.selectionGroup renderSelectionItem (itemProps x) x ]
    renderSelections [] =
      HH.div_ []
    renderSelections xs =
      ItemContainer.selectionContainer
        $ (\x -> ItemContainer.selectionGroup renderSelectionItem (itemProps x) x) <$> xs

    renderSearch sst =
      HH.label
        [ HP.classes Input.inputOuterClasses ]
        [ Input.input
            ( S.getInputProps $
              [ HP.value sst.search ]
              <> props
            )
         , HH.span
            [ HP.classes $ Input.inputRightClasses <> Type.linkClasses ]
             [ HH.text "Browse" ]
        ]

    renderContainer cst =
      HH.div
        [ HP.class_ $ HH.ClassName "relative" ]
        if not cst.open then []
        else [ ItemContainer.itemContainer cst.highlightedIndex (renderFuzzy <$> cst.items) ]

