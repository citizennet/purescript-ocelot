module Ocelot.Components.Typeahead.Input where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (class MonadAff)
import DOM.HTML.Indexed (HTMLinput)
import Data.Fuzzy (Fuzzy)
import Data.Maybe (Maybe(..), maybe)
import Data.StrMap (StrMap, fromFoldable, singleton)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Ocelot.Block.Icon as Icon
import Network.RemoteData (RemoteData(..), isSuccess)
import Ocelot.Block.Input as Input
import Ocelot.Block.ItemContainer as ItemContainer
import Ocelot.Block.Format as Format
import Ocelot.Components.Typeahead as TA
import Ocelot.Core.Utils ((<&>))
import Select as Select
import Select.Utils.Setters as Setters


----------
-- Input types expected. This needs to be defined for each 'item' type we have.

type RenderTypeaheadItem o item eff =
  { toStrMap :: item -> StrMap String
  , renderContainer :: RenderContainer o item eff
  , renderItem :: item -> HH.PlainHTML
  }

renderItemString :: ∀ o eff. RenderTypeaheadItem o String eff
renderItemString =
  { toStrMap: singleton "name"
  , renderContainer: defRenderContainer' defRenderFuzzy
  , renderItem: HH.text
  }

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

type RenderContainer o item eff
  = Select.State (Fuzzy item) (TA.Effects eff)
  -> H.ComponentHTML (Select.Query o (Fuzzy item) (TA.Effects eff))

defRenderContainer
  :: ∀ o item eff
   . (Fuzzy item -> HH.PlainHTML)
  -> Array (H.HTML Void (Select.Query o (Fuzzy item) (TA.Effects eff)))
  -> RenderContainer o item eff
defRenderContainer renderFuzzy addlHTML selectState =
  HH.div
    [ HP.class_ $ HH.ClassName "relative" ]
    if selectState.visibility == Select.Off
      then []
      else
        [ ItemContainer.itemContainer
            selectState.highlightedIndex
            (renderFuzzy <$> selectState.items)
            addlHTML
        ]

defRenderContainer'
  :: ∀ o item eff
   . (Fuzzy item -> HH.PlainHTML)
  -> RenderContainer o item eff
defRenderContainer' renderFuzzy = defRenderContainer renderFuzzy []


----------
-- Default typeahead configurations

-- A def single-select that is provided with a renderFuzzy and renderItem function.
defSingle :: ∀ o item err eff m
  . MonadAff (TA.Effects eff) m
 => Eq item
 => Show err
 => Array (H.IProp HTMLinput (Select.Query o (Fuzzy item) (TA.Effects eff)))
 -> Array item
 -> RenderTypeaheadItem o item eff
 -> TA.Input o item err (TA.Effects eff) m
defSingle props xs { toStrMap, renderContainer, renderItem } =
  { items: Success xs
  , search: Nothing
  , initialSelection: TA.One Nothing
  , render: renderTA props renderContainer renderItem
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
 -> RenderTypeaheadItem o item eff
 -> TA.Input o item err (TA.Effects eff) m
defLimit props n xs { toStrMap, renderContainer, renderItem } =
  { items: Success xs
  , search: Nothing
  , initialSelection: TA.Limit n []
  , render: renderTA props renderContainer renderItem
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
 -> RenderTypeaheadItem o item eff
 -> TA.Input o item err (TA.Effects eff) m
defMulti props xs { toStrMap, renderContainer, renderItem } =
  { items: Success xs
  , search: Nothing
  , initialSelection: TA.Many []
  , render: renderTA props renderContainer renderItem
  , config: syncConfig toStrMap true
  }

-- A def async single select using the default render function
defAsyncSingle :: ∀ o item err eff m
  . MonadAff (TA.Effects eff) m
  => Eq item
  => Show err
  => Array (H.IProp HTMLinput (Select.Query o (Fuzzy item) (TA.Effects eff)))
  -> (String -> Aff (TA.Effects eff) (RemoteData err (Array item)))
  -> RenderTypeaheadItem o item eff
  -> TA.Input o item err (TA.Effects eff) m
defAsyncSingle props f { toStrMap, renderContainer, renderItem } =
  { items: NotAsked
  , search: Nothing
  , initialSelection: TA.One Nothing
  , render: renderTA props renderContainer renderItem
  , config: asyncConfig (Milliseconds 800.0) f toStrMap false
  }

-- A def multi-select using the default render item function
defAsyncMulti :: ∀ o item err eff m
  . MonadAff (TA.Effects eff) m
 => Eq item
 => Show err
 => Array (H.IProp HTMLinput (Select.Query o (Fuzzy item) (TA.Effects eff)))
 -> (String -> Aff (TA.Effects eff) (RemoteData err (Array item)))
 -> RenderTypeaheadItem o item eff
 -> TA.Input o item err (TA.Effects eff) m
defAsyncMulti props f { toStrMap, renderContainer, renderItem } =
  { items: NotAsked
  , search: Nothing
  , initialSelection: TA.Many []
  , render: renderTA props renderContainer renderItem
  , config: asyncConfig (Milliseconds 800.0) f toStrMap true
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
 -> RenderContainer o item eff
 -> (item -> HH.PlainHTML)
 -> TA.State item err (TA.Effects eff)
 -> TAParentHTML o item err (TA.Effects eff) m
renderTA props renderContainer renderSelectionItem st =
  renderSlot $
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
      , render: \selectState -> HH.div_ [ renderSearch, renderContainer_ selectState ]
      }

    renderSlot =
      case st.selections of
        TA.One x      -> renderSingle x
        TA.Many xs    -> renderMulti xs
        TA.Limit _ xs -> renderMulti xs

    render selectState =
      HH.div_
        [ renderSearch
        , renderContainer selectState
        ]

    renderSearch =
      case st.selections of
        TA.One x  -> renderSingleSearch x
        otherwise -> renderMultiSearch

    renderSingle x slot =
      HH.label_
        [ Input.inputGroup
          [ HP.class_ $ HH.ClassName $ maybe "offscreen" (const "") x ]
          ( ( maybe [] pure $ renderSingleItem <$> x ) <>
            [ Input.borderRight
              [ HP.classes Format.linkClasses ]
              [ HH.text "Change" ]
            ]
          )
        , HH.div
          [ HP.class_ $ HH.ClassName $ maybe "" (const "offscreen") x ]
          [ slot ]
        ]

    renderSingleItem x =
      HH.div
        [ HP.classes Input.mainLeftClasses ]
        [ renderSelectionItem' x ]

    renderSingleSearch x =
      Input.inputGroup_
        [ Input.inputCenter
          ( [ HP.class_ $ HH.ClassName "focus:next:text-blue-88" ]
            <&> Setters.setInputProps props
          )
        , Input.addonCenter
          [ HP.class_
            $ HH.ClassName
            $ case st.items of
                Loading -> ""
                otherwise -> "offscreen"
          ]
          [ Icon.loading_ ]
        , Input.addonLeft_ [ Icon.search_ ]
        , Input.borderRight
          [ HP.classes Format.linkClasses ]
          [ HH.text "Browse" ]
        ]

    renderMulti xs slot =
      HH.div_
        ( [ ItemContainer.selectionContainer ( renderSelectionItem' <$> xs )
          , slot
          ]
        )

    renderMultiSearch =
      Input.inputGroup_
        [ Input.inputCenter
          ( [ HP.class_ $ HH.ClassName "focus:next:text-blue-88" ]
            <&> Setters.setInputProps props
          )
        , Input.addonCenter
          [ HP.class_
            $ HH.ClassName
            $ case st.items of
                Loading -> ""
                otherwise -> "offscreen"
          ]
          [ Icon.loading_ ]
        , Input.addonLeft_ [ Icon.search_ ]
        , Input.borderRight
          [ HP.classes Format.linkClasses ]
          [ HH.text "Browse" ]
        ]

    renderSelectionItem' x =
      ItemContainer.selectionGroup
        renderSelectionItem [ HE.onClick $ HE.input_ $ TA.Remove x ] x

    renderContainer_
      | isSuccess st.items = renderContainer
      | otherwise = const $ HH.div_ []
