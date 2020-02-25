module Ocelot.Component.Typeahead.Render where

import Prelude

import DOM.HTML.Indexed (HTMLinput)
import Data.Array (foldr, (:))
import Data.Array as Array
import Data.Fuzzy (Fuzzy)
import Data.Maybe (Maybe(..), isJust, isNothing, maybe)
import Data.Newtype (unwrap)
import Halogen.HTML as HH
import Halogen.HTML.Core (Prop(..), PropValue)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.RemoteData (isFailure, isLoading)
import Ocelot.Block.Button as Button
import Ocelot.Block.Conditional (conditional)
import Ocelot.Block.Format as Format
import Ocelot.Block.Icon as Icon
import Ocelot.Block.Input as Input
import Ocelot.Block.ItemContainer as IC
import Ocelot.Block.Loading as Loading
import Ocelot.Component.Typeahead.Base as TA
import Ocelot.HTML.Properties (css, (<&>))
import Select as S
import Select.Setters (setInputProps) as Setters
import Unsafe.Coerce (unsafeCoerce)

----------
-- Overall Rendering

renderSingle
  :: ∀ action item m
  . Array (HP.IProp HTMLinput (TA.CompositeAction action Maybe item m))
  -> (item -> HH.PlainHTML)
  -> TA.CompositeComponentRender action Maybe item m
  -> TA.CompositeComponentRender action Maybe item m
renderSingle iprops renderItem renderContainer st =
  HH.div_
    [ Input.inputGroup' HH.div
      [ css $ if showSelected then "" else "offscreen" ]
      [ if disabled
          then
            maybe (HH.text "")
              ( \selected -> HH.div
                [ HP.classes disabledClasses ]
                [ HH.fromPlainHTML $ renderItem selected ]
              )
            st.selected
          else
            maybe (HH.text "")
            ( \selected -> HH.div
              [ HP.classes Input.mainLeftClasses ]
              [ IC.selectionGroup renderItem []
                [ HE.onClick $ const <<< Just <<< S.Action $ TA.Remove selected ]
                selected
              ])
            st.selected
      , Input.borderRight
        [ HP.classes $ linkClasses disabled ]
        [ HH.text "Change" ]
      ]
    , Input.inputGroup
      [ css $ if showSelected then "offscreen" else "" ]
      [ Input.inputCenter $ inputProps disabled iprops
      , Input.addonLeft_
        [ Icon.search_ ]
      , Input.addonCenter
        [ css $ if isLoading st.items then "" else "offscreen" ]
        [ spinner ]
      , Input.borderRight
        [ HP.classes $ linkClasses disabled ]
        [ HH.text "Browse" ]
      ]
    , conditional (st.visibility == S.On)
        [ css "relative block" ]
        [ renderContainer st ]
    , renderError $ isFailure st.items
    ]

  where

  disabled = isDisabled iprops
  showSelected = isJust st.selected && st.visibility == S.Off


renderMulti
  :: ∀ action item m
  . Array (HP.IProp HTMLinput (TA.CompositeAction action Array item m))
  -> (item -> HH.PlainHTML)
  -> TA.CompositeComponentRender action Array item m
  -> TA.CompositeComponentRender action Array item m
renderMulti iprops renderItem renderContainer st =
  HH.div
    [ css "relative" ]
    [ if (not disabled && not Array.null st.selected)
        then
          HH.a
            [ css "absolute -mt-7 pin-r underline text-grey-70 cursor-pointer"
            , HE.onClick $ const <<< Just <<< S.Action $ TA.RemoveAll
            ]
            [ HH.text "Remove All" ]
        else
          HH.text ""
    , IC.selectionContainer $ st.selected <#>
        if disabled
          then
            HH.fromPlainHTML <<< renderItem
          else
            \selected ->
              IC.selectionGroup
                renderItem
                []
                [ HE.onClick $ const <<< Just <<< S.Action $ TA.Remove selected ]
                selected
    , Input.inputGroup_
      [ Input.inputCenter $ inputProps disabled iprops
      , Input.addonLeft_
        [ Icon.search_ ]
      , Input.addonCenter
        [ css $ if isLoading st.items then "" else "offscreen" ]
        [ spinner ]
      , Input.borderRight
        [ HP.classes $ linkClasses disabled ]
        [ HH.text "Browse" ]
      ]
    , conditional (st.visibility == S.On)
        [ css "relative block" ]
        [ renderContainer st ]
    , renderError $ isFailure st.items
    ]

  where

  disabled = isDisabled iprops


----------
-- Default Renders

defRenderContainer
    :: ∀ action f item m
  . (Fuzzy item -> HH.PlainHTML)
  -> TA.CompositeComponentRender action f item m
defRenderContainer renderFuzzy st =
  IC.itemContainer st.highlightedIndex (renderFuzzy <$> st.fuzzyItems) []


renderToolbarSearchDropdown
  :: ∀ action item m
   . Eq item
  => String
  -> String
  -> (item -> HH.PlainHTML)
  -> (Fuzzy item -> HH.PlainHTML)
  -> TA.CompositeComponentRender action Maybe item m
renderToolbarSearchDropdown defaultLabel resetLabel renderItem renderFuzzy st =
  renderSearchDropdown resetLabel label renderFuzzy st
  where
    label = IC.dropdownButton
      HH.span
      [ HP.classes
        $ HH.ClassName "whitespace-no-wrap"
        : Button.buttonMainClasses
        <> Button.buttonClearClasses
      ]
      [ maybe (HH.text defaultLabel) (HH.fromPlainHTML <<< renderItem) st.selected ]

renderHeaderSearchDropdown
  :: ∀ action item m
   . Eq item
  => String
  -> String
  -> (item -> HH.PlainHTML)
  -> (Fuzzy item -> HH.PlainHTML)
  -> TA.CompositeComponentRender action Maybe item m
renderHeaderSearchDropdown defaultLabel resetLabel renderItem renderFuzzy st =
  renderSearchDropdown resetLabel label renderFuzzy st
  where
    label = HH.span
      [ css "text-white text-3xl font-thin cursor-pointer whitespace-no-wrap" ]
      [ maybe (HH.text defaultLabel) (HH.fromPlainHTML <<< renderItem) st.selected
      , Icon.collapse [ css "ml-3 text-xl text-grey-50 align-middle" ]
      ]

renderSearchDropdown
  :: ∀ action item m
   . Eq item
  => String
  -> HH.PlainHTML
  -> (Fuzzy item -> HH.PlainHTML)
  -> TA.CompositeComponentRender action Maybe item m
renderSearchDropdown resetLabel label renderFuzzy st =
  HH.label
    [ css "relative" ]
    [ HH.fromPlainHTML label
    , HH.div
      [ HP.classes
        $ HH.ClassName "min-w-80" :
          if st.visibility == S.Off
            then [ HH.ClassName "offscreen" ]
            else []
      ]
      [ IC.dropdownContainer
        [ renderInput, renderReset ]
        renderFuzzy
        ((==) st.selected <<< Just <<< _.original <<< unwrap)
        st.fuzzyItems
        st.highlightedIndex
      ]
    ]
  where
    renderInput =
      HH.div
        [ css "m-4 border-b-2 border-blue-88 pb-2 flex" ]
        [ Icon.search [ css "mr-4 text-xl text-grey-70" ]
        , HH.input
          $ inputProps false [ css "no-outline w-full", HP.placeholder "Search" ]
        ]

    renderReset =
      IC.dropdownItem
        HH.div
        [ HE.onClick $ const <<< Just <<< S.Action $ TA.RemoveAll
        ]
        [ HH.text resetLabel ]
        ( isNothing st.selected )
        false

----------
-- Shared Helpers

linkClasses :: Boolean -> Array HH.ClassName
linkClasses = if _
  then HH.ClassName <$> [ "text-grey-70", "no-underline", "font-medium" ]
  else Format.linkClasses


inputProps
  :: ∀ action f item m
   . Boolean
  -> Array (HP.IProp HTMLinput (TA.CompositeAction action f item m))
  -> Array (HP.IProp HTMLinput (TA.CompositeAction action f item m))
inputProps disabled iprops = if disabled
  then iprops'
  else Setters.setInputProps iprops'
  where
    iprops' = [ HP.autocomplete false, css "focus:next:text-blue-88" ] <&> iprops


disabledClasses :: Array HH.ClassName
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

isDisabled :: ∀ i. Array (HH.IProp HTMLinput i) -> Boolean
isDisabled = foldr f false
  where
    f (HP.IProp (Property "disabled" disabled)) | coercePropValue disabled == true = (||) true
    f _ = (||) false

    coercePropValue :: PropValue -> Boolean
    coercePropValue = unsafeCoerce

renderError :: ∀ p i. Boolean -> HH.HTML p i
renderError error =
  conditional error
    [ css "flex items-center mt-1" ]
    [ Icon.error
      [ css "text-2xl text-yellow" ]
    , HH.p
      [ css "ml-3 text-grey-50 font-light" ]
      [ HH.text "Some data could not be retrieved here." ]
    ]

spinner :: ∀ p i. HH.HTML p i
spinner = Loading.spinner [ css "w-6 text-blue-88" ]
