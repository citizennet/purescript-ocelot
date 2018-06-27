module Ocelot.Components.Typeahead.Input where

import Prelude

import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import DOM.HTML.Indexed (HTMLinput)
import Data.Array (foldr)
import Data.Fuzzy (Fuzzy)
import Data.Maybe (Maybe(..), maybe)
import Foreign.Object (Object, fromFoldable, singleton)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (Prop(..), PropValue)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..), isFailure, isSuccess)
import Ocelot.Block.Format as Format
import Ocelot.Block.Loading as Loading
import Ocelot.Block.Icon as Icon
import Ocelot.Block.Input as Input
import Ocelot.Block.ItemContainer as ItemContainer
import Ocelot.Components.Typeahead as TA
import Ocelot.HTML.Properties ((<&>))
import Select as Select
import Select.Utils.Setters as Setters
import Unsafe.Coerce (unsafeCoerce)


----------
-- Input types expected. This needs to be defined for each 'item' type we have.

type RenderTypeaheadItem o item =
  { toObject :: item -> Object String
  , renderContainer :: RenderContainer o item
  , renderItem :: item -> HH.PlainHTML
  }

renderItemString :: ∀ o. RenderTypeaheadItem o String
renderItemString =
  { toObject: singleton "name"
  , renderContainer: defRenderContainer' defRenderFuzzy
  , renderItem: HH.text
  }

----------
-- Default rendering

defToObject :: ∀ r. { name :: String | r } -> Object String
defToObject { name } = fromFoldable [ Tuple "name" name ]

-- WARNING: This expects you to have a string map with the "name"
-- key present or else it will not work but will compile!
defRenderFuzzy :: ∀ item. Fuzzy item -> HH.PlainHTML
defRenderFuzzy = HH.span_ <<< ItemContainer.boldMatches "name"

defRenderItem :: ∀ r. { name :: String | r } -> HH.PlainHTML
defRenderItem { name } = HH.text name

type RenderContainer o item
  = Select.State (Fuzzy item)
  -> H.ComponentHTML (Select.Query o (Fuzzy item))

defRenderContainer
  :: ∀ o item
   . (Fuzzy item -> HH.PlainHTML)
  -> Array (H.HTML Void (Select.Query o (Fuzzy item)))
  -> RenderContainer o item
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
  :: ∀ o item
   . (Fuzzy item -> HH.PlainHTML)
  -> RenderContainer o item
defRenderContainer' renderFuzzy = defRenderContainer renderFuzzy []


----------
-- Default typeahead configurations

-- A def single-select that is provided with a renderFuzzy and renderItem function.
defSingle :: ∀ o item err m
  . MonadAff m
 => Eq item
 => Show err
 => Array (H.IProp HTMLinput (Select.Query o (Fuzzy item)))
 -> Array item
 -> RenderTypeaheadItem o item
 -> TA.Input o item err m
defSingle props xs { toObject, renderContainer, renderItem } =
  { items: Success xs
  , search: Nothing
  , initialSelection: TA.One Nothing
  , render: renderTA props renderContainer renderItem
  , config: syncConfig toObject false
  }

-- A def multi-select limited to N total possible selections.
defLimit :: ∀ o item err m
  . MonadAff m
 => Eq item
 => Show err
 => Array (H.IProp HTMLinput (Select.Query o (Fuzzy item)))
 -> Int
 -> Array item
 -> RenderTypeaheadItem o item
 -> TA.Input o item err m
defLimit props n xs { toObject, renderContainer, renderItem } =
  { items: Success xs
  , search: Nothing
  , initialSelection: TA.Limit n []
  , render: renderTA props renderContainer renderItem
  , config: syncConfig toObject true
  }

-- A def multi-select that is provided with a renderFuzzy and renderItem function to determine
-- rendering a specific item in the container
defMulti :: ∀ o item err m
  . MonadAff m
 => Eq item
 => Show err
 => Array (H.IProp HTMLinput (Select.Query o (Fuzzy item)))
 -> Array item
 -> RenderTypeaheadItem o item
 -> TA.Input o item err m
defMulti props xs { toObject, renderContainer, renderItem } =
  { items: Success xs
  , search: Nothing
  , initialSelection: TA.Many []
  , render: renderTA props renderContainer renderItem
  , config: syncConfig toObject true
  }

-- A def async single select using the default render function
defAsyncSingle :: ∀ o item err m
  . MonadAff m
  => Eq item
  => Show err
  => Array (H.IProp HTMLinput (Select.Query o (Fuzzy item)))
  -> (String -> Aff (RemoteData err (Array item)))
  -> RenderTypeaheadItem o item
  -> TA.Input o item err m
defAsyncSingle props f { toObject, renderContainer, renderItem } =
  { items: NotAsked
  , search: Nothing
  , initialSelection: TA.One Nothing
  , render: renderTA props renderContainer renderItem
  , config: asyncConfig (Milliseconds 800.0) f toObject false
  }

-- A def multi-select using the default render item function
defAsyncMulti :: ∀ o item err m
  . MonadAff m
 => Eq item
 => Show err
 => Array (H.IProp HTMLinput (Select.Query o (Fuzzy item)))
 -> (String -> Aff (RemoteData err (Array item)))
 -> RenderTypeaheadItem o item
 -> TA.Input o item err m
defAsyncMulti props f { toObject, renderContainer, renderItem } =
  { items: NotAsked
  , search: Nothing
  , initialSelection: TA.Many []
  , render: renderTA props renderContainer renderItem
  , config: asyncConfig (Milliseconds 800.0) f toObject true
  }


----------
-- Default Configuration

syncConfig :: ∀ item err
  . Eq item
 => (item -> Object String)
 -> Boolean
 -> TA.Config item err
syncConfig toObject keepOpen =
  { insertable: TA.NotInsertable
  , filterType: TA.FuzzyMatch
  , syncMethod: TA.Sync
  , toObject
  , keepOpen
  }

asyncConfig :: ∀ item err
  . Eq item
 => Milliseconds
 -> (String -> Aff (RemoteData err (Array item)))
 -> (item -> Object String)
 -> Boolean
 -> TA.Config item err
asyncConfig ms f toObject keepOpen =
  { insertable: TA.NotInsertable
  , filterType: TA.FuzzyMatch
  , syncMethod: TA.Async { debounceTime: ms, fetchItems: f }
  , toObject
  , keepOpen
  }


----------
-- Render function

type TAParentHTML o item err m
  = H.ParentHTML (TA.Query o item err m) (TA.ChildQuery o (Fuzzy item)) TA.ChildSlot m

renderTA :: ∀ o item err m
  . MonadAff m
 => Eq item
 => Array (H.IProp HTMLinput (Select.Query o (Fuzzy item)))
 -> RenderContainer o item
 -> (item -> HH.PlainHTML)
 -> TA.State item err
 -> TAParentHTML o item err m
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
      , render: \selectState -> HH.div_ [ renderSearch, renderContainer' selectState, renderError ]
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
              [ HP.classes linkClasses ]
              [ HH.text "Change" ]
            ]
          )
        , HH.div
          [ HP.class_ $ HH.ClassName $ maybe "" (const "offscreen") x ]
          [ slot ]
        ]

    renderSingleItem x =
      HH.div
        [ HP.classes if isDisabled then disabledClasses else Input.mainLeftClasses ]
        [ renderSelectionItem' x ]

    renderSingleSearch x =
      Input.inputGroup_
        [ Input.inputCenter
          ( [ HP.class_ $ HH.ClassName "focus:next:text-blue-88" ]
            <&> inputProps
          )
        , Input.addonCenter
          [ HP.class_
            $ HH.ClassName
            $ case st.items of
                Loading -> ""
                otherwise -> "offscreen"
          ]
          [ Loading.spinner
            [ HP.classes spinnerClasses ]
          ]
        , Input.addonLeft_ [ Icon.search_ ]
        , Input.borderRight
          [ HP.classes linkClasses ]
          [ HH.text "Browse" ]
        ]

    renderMulti xs slot =
      HH.div
        [ HP.class_ $ HH.ClassName "relative" ]
        ( removeAllBtn <>
          [ ItemContainer.selectionContainer ( renderSelectionItem' <$> xs )
          , slot
          ]
        )
      where
        removeAllBtn = case st.selections of
          TA.Many [] -> []
          TA.Limit _ [] -> []
          _ ->
            [ HH.a
              [ HP.class_ $ HH.ClassName "absolute -mt-7 pin-r underline text-grey-70"
              , HE.onClick $ HE.input_ TA.RemoveAll ]
              [ HH.text "Remove All" ]
            ]

    renderMultiSearch =
      Input.inputGroup_
        [ Input.inputCenter
          ( [ HP.class_ $ HH.ClassName "focus:next:text-blue-88" ]
            <&> inputProps
          )
        , Input.addonCenter
          [ HP.class_
            $ HH.ClassName
            $ case st.items of
                Loading -> ""
                otherwise -> "offscreen"
          ]
          [ Loading.spinner
            [ HP.classes spinnerClasses ]
          ]
        , Input.addonLeft_ [ Icon.search_ ]
        , Input.borderRight
          [ HP.classes linkClasses ]
          [ HH.text "Browse" ]
        ]

    renderSelectionItem' x =
      if isDisabled then
        HH.fromPlainHTML $ renderSelectionItem x
      else
        ItemContainer.selectionGroup
          renderSelectionItem [ HE.onClick $ HE.input_ $ TA.Remove x ] x

    renderContainer'
      | isSuccess st.items = renderContainer
      | otherwise = const $ HH.div_ []

    isDisabled :: Boolean
    isDisabled = foldr f false props
      where
        f (HP.IProp (Property "disabled" disabled)) | coercePropValue disabled == true = (||) true
        f _ = (||) false

        coercePropValue :: PropValue -> Boolean
        coercePropValue = unsafeCoerce

    inputProps
      | isDisabled == true = props
      | otherwise = Setters.setInputProps props

    linkClasses
      | isDisabled == true = [ HH.ClassName "text-grey-70 no-underline font-medium" ]
      | otherwise = Format.linkClasses

    disabledClasses = HH.ClassName <$>
      [ "bg-grey-95"
      , "text-grey-70"
      , "sibling:bg-grey-95"
      , "sibling:text-grey-50"
      , "border-t-2"
      , "border-b-2"
      , "font-light"
      , "focus:no-outline"
      , "py-2"
      , "border-l-2"
      , "w-full"
      , "px-3"
      ]

    spinnerClasses = HH.ClassName <$>
      [ "w-6"
      , "text-blue-88"
      ]

    renderError
      | isFailure st.items =
        HH.div
        [ HP.class_ $ HH.ClassName "flex items-center mt-1" ]
        [ Icon.error
          [ HP.class_ $ HH.ClassName "text-2xl text-yellow" ]
        , HH.p
          [ HP.class_ $ HH.ClassName "ml-3 text-grey-50 font-light" ]
          [ HH.text "Some data could not be retrieved here." ]
        ]
      | otherwise = HH.div_ []
