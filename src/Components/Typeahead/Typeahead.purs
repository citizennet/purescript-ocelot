module Ocelot.Components.Typeahead where

import Prelude

import Data.Fuzzy (Fuzzy)
import Data.Maybe (Maybe(..))
import Data.StrMap (StrMap, fromFoldable, singleton)
import Data.Tuple (Tuple(..))
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (class MonadAff)
import Data.Time.Duration (Milliseconds(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import DOM.HTML.Indexed (HTMLinput)
import Network.RemoteData (RemoteData(..))

import Select as Select
import Select.Utils.Setters as Setters

import Ocelot.Block.ItemContainer as ItemContainer

import Ocelot.Core.Typeahead as TA
import Ocelot.Block.Type as Type
import Ocelot.Block.Input as Input


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
defSingle :: ∀ o item err eff m
  . MonadAff (TA.Effects eff) m
 => Eq item
 => Show err
 => Array (H.IProp HTMLinput (Select.Query o (Fuzzy item) (TA.Effects eff)))
 -> Array item
 -> RenderTypeaheadItem item
 -> TA.Input o item err (TA.Effects eff) m
defSingle props xs { toStrMap, renderFuzzy, renderItem } =
  { items: Success xs
  , search: Nothing
  , initialSelection: TA.One Nothing
  , render: renderTA props renderFuzzy renderItem
  , config: syncConfig toStrMap false
  }

-- A def multi-select limited to N total possible selections.
defLimit :: ∀ o item err eff m
  . MonadAff (TA.Effects eff) m
 => Eq item
 => Show err
 => Array (H.IProp HTMLinput (Select.Query o (Fuzzy item) (TA.Effects eff)))
 -> Int
 -> Array item
 -> RenderTypeaheadItem item
 -> TA.Input o item err (TA.Effects eff) m
defLimit props n xs { toStrMap, renderFuzzy, renderItem } =
  { items: Success xs
  , search: Nothing
  , initialSelection: TA.Limit n []
  , render: renderTA props renderFuzzy renderItem
  , config: syncConfig toStrMap true
  }

-- A def multi-select that is provided with a renderFuzzy and renderItem function to determine
-- rendering a specific item in the container
defMulti :: ∀ o item err eff m
  . MonadAff (TA.Effects eff) m
 => Eq item
 => Show err
 => Array (H.IProp HTMLinput (Select.Query o (Fuzzy item) (TA.Effects eff)))
 -> Array item
 -> RenderTypeaheadItem item
 -> TA.Input o item err (TA.Effects eff) m
defMulti props xs { toStrMap, renderFuzzy, renderItem } =
  { items: Success xs
  , search: Nothing
  , initialSelection: TA.Many []
  , render: renderTA props renderFuzzy renderItem
  , config: syncConfig toStrMap true
  }

-- A def async single select using the default render function
defAsyncSingle :: ∀ o item err eff m
  . MonadAff (TA.Effects eff) m
  => Eq item
  => Show err
  => Array (H.IProp HTMLinput (Select.Query o (Fuzzy item) (TA.Effects eff)))
  -> (String -> Aff (TA.Effects eff) (RemoteData err (Array item)))
  -> RenderTypeaheadItem item
  -> TA.Input o item err (TA.Effects eff) m
defAsyncSingle props f { toStrMap, renderFuzzy, renderItem } =
  { items: NotAsked
  , search: Nothing
  , initialSelection: TA.One Nothing
  , render: renderTA props renderFuzzy renderItem
  , config: asyncConfig (Milliseconds 100.0) f toStrMap true
  }

-- A def multi-select using the default render item function
defAsyncMulti :: ∀ o item err eff m
  . MonadAff (TA.Effects eff) m
 => Eq item
 => Show err
 => Array (H.IProp HTMLinput (Select.Query o (Fuzzy item) (TA.Effects eff)))
 -> (String -> Aff (TA.Effects eff) (RemoteData err (Array item)))
 -> RenderTypeaheadItem item
 -> TA.Input o item err (TA.Effects eff) m
defAsyncMulti props f { toStrMap, renderFuzzy, renderItem } =
  { items: NotAsked
  , search: Nothing
  , initialSelection: TA.Many []
  , render: renderTA props renderFuzzy renderItem
  , config: asyncConfig (Milliseconds 100.0) f toStrMap false
  }


----------
-- Default Configuration

syncConfig :: ∀ item err eff
  . Eq item
 => (item -> StrMap String)
 -> Boolean
 -> TA.Config item err (TA.Effects eff)
syncConfig toStrMap keepOpen =
  { insertable: TA.NotInsertable
  , filterType: TA.FuzzyMatch
  , syncMethod: TA.Sync
  , toStrMap
  , keepOpen
  }

asyncConfig :: ∀ item err eff
  . Eq item
 => Milliseconds
 -> (String -> Aff (TA.Effects eff) (RemoteData err (Array item)))
 -> (item -> StrMap String)
 -> Boolean
 -> TA.Config item err (TA.Effects eff)
asyncConfig ms f toStrMap keepOpen =
  { insertable: TA.NotInsertable
  , filterType: TA.FuzzyMatch
  , syncMethod: TA.Async { debounceTime: ms, fetchItems: f }
  , toStrMap
  , keepOpen
  }


----------
-- Render function

type TAParentHTML o item err eff m
  = H.ParentHTML (TA.Query o item err eff m) (TA.ChildQuery o (Fuzzy item) eff) TA.ChildSlot m

renderTA :: ∀ o item err eff m
  . MonadAff (TA.Effects eff) m
 => Eq item
 => Array (H.IProp HTMLinput (Select.Query o (Fuzzy item) (TA.Effects eff)))
 -> (Fuzzy item -> HH.PlainHTML)
 -> (item -> HH.PlainHTML)
 -> TA.State item err (TA.Effects eff)
 -> TAParentHTML o item err (TA.Effects eff) m
renderTA props renderFuzzy renderSelectionItem st =
  renderAll $
    HH.slot
      unit
      Select.component
      selectInput
      (HE.input TA.HandleSelect)
  where
    selectInput =
      { inputType: Select.TextInput
      , items: []
      , initialSearch: Nothing
      , debounceTime: case st.config.syncMethod of
          TA.Async { debounceTime } -> Just debounceTime
          TA.Sync -> Nothing
      , render: \selectState -> HH.div_ [ renderSearch, renderContainer selectState ]
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

    renderSearch =
      HH.label
        [ HP.classes Input.inputOuterClasses ]
        [ Input.input
            ( Setters.setInputProps props )
         , HH.span
            [ HP.classes $ Input.inputRightClasses <> Type.linkClasses ]
             [ HH.text "Browse" ]
        ]

    renderContainer selectState =
      HH.div
        [ HP.class_ $ HH.ClassName "relative" ]
        if selectState.visibility == Select.Off then []
        else [ ItemContainer.itemContainer selectState.highlightedIndex (renderFuzzy <$> selectState.items) ]

