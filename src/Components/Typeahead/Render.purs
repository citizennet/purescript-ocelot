module Ocelot.Component.Typeahead.Render where

import Prelude

import DOM.HTML.Indexed (HTMLinput)
import Data.Array (foldr, null, (:))
import Data.Fuzzy (Fuzzy)
import Data.Maybe (Maybe(..), isJust, isNothing, maybe)
import Data.Newtype (unwrap)
import Halogen as H
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
import Select as Select
import Select.Setters (setInputProps) as Setters
import Unsafe.Coerce (unsafeCoerce)

----------
-- Overall Rendering

renderSingle
  :: ∀ pq item m
   . Array (H.IProp HTMLinput (Select.Query (TA.Query pq Maybe item m) (Fuzzy item)))
  -> (item -> HH.PlainHTML)
  -> (Select.State (Fuzzy item) -> Select.ComponentHTML (TA.Query pq Maybe item m) (Fuzzy item))
  -> TA.State Maybe item m
  -> Select.State (Fuzzy item)
  -> Select.ComponentHTML (TA.Query pq Maybe item m) (Fuzzy item)
renderSingle iprops renderItem renderContainer pst cst =
  HH.div_
    [ Input.inputGroup' HH.div
      [ HE.onClick $ Select.always $ Select.raise $ TA.TriggerFocus unit
      , HP.class_ $ HH.ClassName (if showSelected then "" else "offscreen")
      ]
      [ if disabled
          then
            maybe (HH.text "")
              ( \selected -> HH.div
                [ HP.classes disabledClasses ]
                [ HH.fromPlainHTML $ renderItem selected ]
              )
            pst.selected
          else
            maybe (HH.text "")
            ( \selected -> HH.div
              [ HP.classes Input.mainLeftClasses ]
              [ IC.selectionGroup renderItem []
                [ HE.onClick
                  $ Select.always
                  $ Select.raise
                  $ TA.AndThen (TA.Remove selected unit) (TA.TriggerFocus unit) unit
                ]
                selected
              ])
            pst.selected
      , Input.borderRight
        [ HP.classes $ linkClasses disabled ]
        [ HH.text "Change" ]
      ]
    , Input.inputGroup
      [ HP.class_ $ HH.ClassName (if showSelected then "offscreen" else "")
      , HE.onClick $ Select.always $ Select.raise $ TA.TriggerFocus unit
      ]
      [ Input.inputCenter $ inputProps disabled iprops
      , Input.addonLeft_
        [ Icon.search_ ]
      , Input.addonCenter
        [ css $ if isLoading pst.items then "" else "offscreen" ]
        [ spinner ]
      , Input.borderRight
        [ HP.classes $ linkClasses disabled ]
        [ HH.text "Browse" ]
      ]
    , conditional (cst.visibility == Select.On)
        [ css "relative block" ]
        [ renderContainer cst ]
    , renderError $ isFailure pst.items
    ]

  where

  disabled = isDisabled iprops
  showSelected = isJust pst.selected && cst.visibility == Select.Off


renderMulti
  :: ∀ pq item m
   . Array (H.IProp HTMLinput (Select.Query (TA.Query pq Array item m) (Fuzzy item)))
  -> (item -> HH.PlainHTML)
  -> (Select.State (Fuzzy item) -> Select.ComponentHTML (TA.Query pq Array item m) (Fuzzy item))
  -> TA.State Array item m
  -> Select.State (Fuzzy item)
  -> Select.ComponentHTML (TA.Query pq Array item m) (Fuzzy item)
renderMulti iprops renderItem renderContainer pst cst =
  HH.div
    [ css "relative" ]
    [ if (not disabled && not null pst.selected)
        then
          HH.a
            [ css "absolute -mt-7 pin-r underline text-grey-70 cursor-pointer"
            , HE.onClick $ Select.always $ Select.raise $ TA.RemoveAll unit
            ]
            [ HH.text "Remove All" ]
        else
          HH.text ""
    , IC.selectionContainer $ pst.selected <#>
        if disabled
          then
            HH.fromPlainHTML <<< renderItem
          else
            \selected ->
              IC.selectionGroup
                renderItem
                []
                [ HE.onClick $ Select.always $ Select.raise $ TA.Remove selected unit ]
                selected
    , Input.inputGroup_
      [ Input.inputCenter $ inputProps disabled iprops
      , Input.addonLeft_
        [ Icon.search_ ]
      , Input.addonCenter
        [ css $ if isLoading pst.items then "" else "offscreen" ]
        [ spinner ]
      , Input.borderRight
        [ HP.classes $ linkClasses disabled
        , HE.onClick $ Select.always $ Select.raise $ TA.TriggerFocus unit ]
        [ HH.text "Browse" ]
      ]
    , conditional (cst.visibility == Select.On)
        [ css "relative block" ]
        [ renderContainer cst ]
    , renderError $ isFailure pst.items
    ]

  where

  disabled = isDisabled iprops


----------
-- Default Renders

defRenderContainer
  :: ∀ pq f item m
   . (Fuzzy item -> HH.PlainHTML)
  -> Select.State (Fuzzy item)
  -> Select.ComponentHTML (TA.Query pq f item m) (Fuzzy item)
defRenderContainer renderFuzzy cst =
  IC.itemContainer cst.highlightedIndex (renderFuzzy <$> cst.items) []

renderToolbarSearchDropdown
  :: ∀ pq item m
   . Eq item
  => String
  -> String
  -> (item -> HH.PlainHTML)
  -> (Fuzzy item -> HH.PlainHTML)
  -> TA.State Maybe item m
  -> Select.State (Fuzzy item)
  -> Select.ComponentHTML (TA.Query pq Maybe item m) (Fuzzy item)
renderToolbarSearchDropdown defaultLabel resetLabel renderItem renderFuzzy pst cst =
  renderSearchDropdown resetLabel label renderFuzzy pst cst
  where
    label = IC.dropdownButton
      HH.span
      [ HP.classes
        $ HH.ClassName "whitespace-no-wrap"
        : Button.buttonMainClasses
        <> Button.buttonClearClasses
      ]
      [ maybe (HH.text defaultLabel) (HH.fromPlainHTML <<< renderItem) pst.selected ]

renderHeaderSearchDropdown
  :: ∀ pq item m
   . Eq item
  => String
  -> String
  -> (item -> HH.PlainHTML)
  -> (Fuzzy item -> HH.PlainHTML)
  -> TA.State Maybe item m
  -> Select.State (Fuzzy item)
  -> Select.ComponentHTML (TA.Query pq Maybe item m) (Fuzzy item)
renderHeaderSearchDropdown defaultLabel resetLabel renderItem renderFuzzy pst cst =
  renderSearchDropdown resetLabel label renderFuzzy pst cst
  where
    label = HH.span
      [ css "text-white text-3xl font-thin cursor-pointer whitespace-no-wrap" ]
      [ maybe (HH.text defaultLabel) (HH.fromPlainHTML <<< renderItem) pst.selected
      , Icon.collapse [ css "ml-3 text-xl text-grey-50 align-middle" ]
      ]

renderSearchDropdown
  :: ∀ pq item m
   . Eq item
  => String
  -> HH.PlainHTML
  -> (Fuzzy item -> HH.PlainHTML)
  -> TA.State Maybe item m
  -> Select.State (Fuzzy item)
  -> Select.ComponentHTML (TA.Query pq Maybe item m) (Fuzzy item)
renderSearchDropdown resetLabel label renderFuzzy pst cst =
  HH.label
    [ css "relative" ]
    [ HH.fromPlainHTML label
    , HH.div
      [ HP.classes
        $ HH.ClassName "min-w-80" :
          if cst.visibility == Select.Off
            then [ HH.ClassName "offscreen" ]
            else []
      ]
      [ IC.dropdownContainer
        [ renderInput, renderReset ]
        renderFuzzy
        ((==) pst.selected <<< Just <<< _.original <<< unwrap)
        cst.items
        cst.highlightedIndex
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
        [ HE.onClick \_ -> Just do
          Select.raise $ TA.RemoveAll unit
          Select.setVisibility Select.Off
        ]
        [ HH.text resetLabel ]
        ( isNothing pst.selected )
        false
----------
-- Shared Helpers

linkClasses :: Boolean -> Array HH.ClassName
linkClasses = if _
  then HH.ClassName <$> [ "text-grey-70", "no-underline", "font-medium" ]
  else Format.linkClasses

inputProps
  :: ∀ pq f item m
   . Boolean
  -> Array (H.IProp HTMLinput (Select.Query (TA.Query pq f item m) (Fuzzy item)))
  -> Array (H.IProp HTMLinput (Select.Query (TA.Query pq f item m) (Fuzzy item)))
inputProps disabled iprops = if disabled
  then iprops'
  else Setters.setInputProps iprops'
  where iprops' = [ HP.autocomplete false, css "focus:next:text-blue-88" ] <&> iprops


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
