-- | A JavaScript interface for the Ocelot typeahead, which
-- | only supports sync typeaheads.
module Ocelot.Interface.Typeahead where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Array (head)
import Data.Fuzzy (match)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromJust, fromMaybe, maybe)
import Data.Symbol (SProxy(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Variant (Variant, inj)
import Effect (Effect)
import Effect.AVar as AVar
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.AVar as AffAVar
import Effect.Aff.Compat (EffectFn1, EffectFn2, mkEffectFn1, mkEffectFn2)
import Foreign.Object (Object)
import Foreign.Object as Object
import Halogen.HTML (span_)
import Halogen.HTML (text) as HH
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Html.Parser.Halogen as Parser
import Network.RemoteData (RemoteData(..))
import Ocelot.Block.ItemContainer (boldMatches)
import Ocelot.Component.Typeahead (Component, Input, Insertable(..), Message(..), Query(..), defRenderContainer, multi, renderMulti, renderSingle, single, base)
import Ocelot.Component.Typeahead.Render (renderHeaderSearchDropdown, renderSearchDropdown, renderToolbarSearchDropdown)
import Ocelot.Interface.Utilities (Interface, mkSubscription)
import Partial.Unsafe (unsafePartial)
import Prim.TypeError (class Warn, Text)
import Web.HTML (HTMLElement)

-- | A subset of the queries available to the typeahead, restricted
-- | to queries that would be sensible to trigger externally.
type QueryRow =
  ( remove :: EffectFn1 (Object String) (Promise Unit)
  , removeAll :: Effect (Promise Unit)
  , triggerFocus :: Effect (Promise Unit)
  , search :: EffectFn1 String (Promise Unit)
  , getSelected :: Effect (Promise (Array (Object String)))
  , setSelected :: EffectFn1 (Array (Object String)) (Promise Unit)
  , setItems :: EffectFn1 (Array (Object String)) (Promise Unit)
  , setError :: EffectFn1 String (Promise Unit)
  , setLoading :: Effect (Promise Unit)
  , reset :: Effect (Promise Unit)
  )

-- | A subset of the messages available to the typeahead, restricted
-- | to those that would be sensible to consume externally
type MessageVariant = Variant
  ( searched :: String
  , selected :: Object String
  , selectionChanged :: Array (Object String)
  , emit :: String
  )

convertMultiToMessageVariant :: ∀ pq. Message pq Array (Object String) -> MessageVariant
convertMultiToMessageVariant = case _ of
  Searched str -> inj (SProxy :: SProxy "searched") str
  Selected obj -> inj (SProxy :: SProxy "selected") obj
  SelectionChanged _ arr -> inj (SProxy :: SProxy "selectionChanged") arr
  Emit _ -> inj (SProxy :: SProxy "emit") "emitted"

convertSingleToMessageVariant :: ∀ pq. Message pq Maybe (Object String) -> MessageVariant
convertSingleToMessageVariant = case _ of
  Searched str -> inj (SProxy :: SProxy "searched") str
  Selected obj -> inj (SProxy :: SProxy "selected") obj
  SelectionChanged _ (Just i) -> inj (SProxy :: SProxy "selectionChanged") [i]
  SelectionChanged _ Nothing -> inj (SProxy :: SProxy "selectionChanged") []
  Emit _ -> inj (SProxy :: SProxy "emit") "emitted"

-- | A subset of the input available to the typeahead
-- | Provide:
-- | - items: an array of objects, where all keys and values are strings
-- | - debounceTime: how long to debounce user input, in milliseconds
-- | - placeholder: placeholder text to set in the field
-- | - key: the name of the field in the object that should be displayed in the list
-- | - keepOpen: whether the typeahead should stay open or close on selection
type TypeaheadInput =
  { items :: Array (Object String)
  , debounceTime :: Int
  , placeholder :: String
  , key :: String
  , keepOpen :: Boolean
  , insertable :: Boolean
  }

-- | Another subset of the input available to the typeahead, for the dropdown variant
-- | Provide:
-- | - items: an array of objects, where all keys and values are strings
-- | - labelHTML: a function that takes a String (the selected item) and returns a string of HTML
-- |   (the toggling element)
-- | - key: the name of the field in the object that should be displayed in the list
-- | - defaultLabel: the string to display when no item is selected
-- | - resetLabel: the name of the container element which resets the selection to Nothing
type DropdownInput =
  { items :: Array (Object String)
  , labelHTML :: (String -> String)
  , key :: String
  , defaultLabel :: String
  , resetLabel :: String
  }

-- | An adapter to simplify types necessary in JS to control the typeahead
-- | component. Items are specialized to Object String.
typeaheadInputToSingleInput
  :: ∀ pq m
   . TypeaheadInput
  -> Input pq Maybe (Object String) m
typeaheadInputToSingleInput r =
  { items: Success r.items
  , insertable: if r.insertable then Insertable (Object.singleton r.key) else NotInsertable
  , keepOpen: r.keepOpen
  , itemToObject: \a -> Object.singleton r.key (unsafePartial (fromJust (Object.lookup r.key a)))
  , debounceTime: if r.debounceTime > 0 then Just (Milliseconds (toNumber r.debounceTime)) else Nothing
  , async: Nothing
  , render: renderSingle
      [ HP.placeholder r.placeholder ]
      (renderFuzzy <<< match false identity "")
      (defRenderContainer renderFuzzy)
  }
  where
    renderFuzzy = span_ <<< boldMatches r.key

typeaheadInputToMultiInput
  :: ∀ pq m
   . TypeaheadInput
  -> Input pq Array (Object String) m
typeaheadInputToMultiInput r =
  { items: Success r.items
  , insertable: if r.insertable then Insertable (Object.singleton r.key) else NotInsertable
  , keepOpen: r.keepOpen
  , itemToObject: \a -> Object.singleton r.key (unsafePartial (fromJust (Object.lookup r.key a)))
  , debounceTime: if r.debounceTime > 0 then Just (Milliseconds (toNumber r.debounceTime)) else Nothing
  , async: Nothing
  , render: renderMulti
      [ HP.placeholder r.placeholder ]
      (renderFuzzy <<< match false identity "")
      (defRenderContainer renderFuzzy)
  }
  where
    renderFuzzy = span_ <<< boldMatches r.key

dropdownInputToSingleInput
  :: ∀ pq m
   . DropdownInput
  -> Input pq Maybe (Object String) m
dropdownInputToSingleInput r =
  { items: Success r.items
  , insertable: NotInsertable
  , keepOpen: false
  , itemToObject: \a -> Object.singleton r.key (unsafePartial (fromJust (Object.lookup r.key a)))
  , debounceTime: Nothing
  , async: Nothing
  , render
  }
  where
    renderFuzzy = span_ <<< boldMatches r.key

    render pst = renderSearchDropdown
      r.resetLabel
      (Parser.render $ r.labelHTML (fromMaybe r.defaultLabel (Object.lookup r.key =<< pst.selected)))
      renderFuzzy
      pst

mountMultiTypeahead :: EffectFn2 HTMLElement TypeaheadInput (Interface MessageVariant QueryRow)
mountMultiTypeahead = mkEffectFn2 \el ext -> do
  ioVar <- AVar.empty
  launchAff_ do
    io <- runUI multi (typeaheadInputToMultiInput ext) el
    AffAVar.put io ioVar
  pure
    { subscribe: mkSubscription ioVar convertMultiToMessageVariant
    , remove: mkEffectFn1 \obj -> Promise.fromAff do
        io <- AffAVar.read ioVar
        io.query $ Remove obj unit
    , removeAll: Promise.fromAff do
        io <- AffAVar.read ioVar
        io.query $ RemoveAll unit
    , triggerFocus: Promise.fromAff do
        io <- AffAVar.read ioVar
        io.query $ TriggerFocus unit
    , search: mkEffectFn1 \str -> Promise.fromAff do
        io <- AffAVar.read ioVar
        io.query $ Search str unit
    , getSelected: Promise.fromAff do
        io <- AffAVar.read ioVar
        io.query $ GetSelected identity
    -- Different from underlying implementation because no algebraic data types
    -- in JS
    , setSelected: mkEffectFn1 \arr -> Promise.fromAff do
        io <- AffAVar.read ioVar
        io.query $ ReplaceSelected arr unit
    -- New queries to support setting status, error, and items separately
    , setItems: mkEffectFn1 \arr -> Promise.fromAff do
        io <- AffAVar.read ioVar
        io.query $ ReplaceItems (Success arr) unit
    , setError: mkEffectFn1 \str -> Promise.fromAff do
        io <- AffAVar.read ioVar
        io.query $ ReplaceItems (Failure str) unit
    , setLoading: Promise.fromAff do
        io <- AffAVar.read ioVar
        io.query $ ReplaceItems Loading unit
    , reset: Promise.fromAff do
        io <- AffAVar.read ioVar
        io.query $ Reset unit
    }

mountSingleTypeahead :: EffectFn2 HTMLElement TypeaheadInput (Interface MessageVariant QueryRow)
mountSingleTypeahead = mkSingleTypeaheadMounter single typeaheadInputToSingleInput

mountDropdownTypeahead :: EffectFn2 HTMLElement DropdownInput (Interface MessageVariant QueryRow)
mountDropdownTypeahead = mkSingleTypeaheadMounter single' dropdownInputToSingleInput

mkSingleTypeaheadMounter
  :: ∀ input pq
   . Component pq Maybe (Object String) Aff
  -> (input -> Input pq Maybe (Object String) Aff)
  -> EffectFn2 HTMLElement input (Interface MessageVariant QueryRow)
mkSingleTypeaheadMounter component inputTransformer = mkEffectFn2 \el ext -> do
  ioVar <- AVar.empty
  launchAff_ do
    io <- runUI component (inputTransformer ext) el
    AffAVar.put io ioVar
  pure
    { subscribe: mkSubscription ioVar convertSingleToMessageVariant
    , remove: mkEffectFn1 \obj -> Promise.fromAff do
        io <- AffAVar.read ioVar
        io.query $ Remove obj unit
    , removeAll: Promise.fromAff do
        io <- AffAVar.read ioVar
        io.query $ RemoveAll unit
    , triggerFocus: Promise.fromAff do
        io <- AffAVar.read ioVar
        io.query $ TriggerFocus unit
    , search: mkEffectFn1 \str -> Promise.fromAff do
        io <- AffAVar.read ioVar
        io.query $ Search str unit
    , getSelected: Promise.fromAff do
        io <- AffAVar.read ioVar
        res <- io.query $ GetSelected identity
        pure $ maybe [] pure res
    , setSelected: mkEffectFn1 \arr -> Promise.fromAff do
        io <- AffAVar.read ioVar
        io.query $ ReplaceSelected (head arr) unit
    -- New queries to support setting status, error, and items separately
    , setItems: mkEffectFn1 \arr -> Promise.fromAff do
        io <- AffAVar.read ioVar
        io.query $ ReplaceItems (Success arr) unit
    , setError: mkEffectFn1 \str -> Promise.fromAff do
        io <- AffAVar.read ioVar
        io.query $ ReplaceItems (Failure str) unit
    , setLoading: Promise.fromAff do
        io <- AffAVar.read ioVar
        io.query $ ReplaceItems Loading unit
    , reset: Promise.fromAff do
        io <- AffAVar.read ioVar
        io.query $ Reset unit
    }

single' :: ∀ pq item. Eq item => Component pq Maybe item Aff
single' = base
  { runSelect: const <<< Just
  , runRemove: const (const Nothing)
  , runFilter: const
  }

-- DEPRECATED
-- Use DropdownInput with dropdownInputToSingleInput
type SearchDropdownInput =
  { items :: Array (Object String)
  , placeholder :: String
  , resetLabel :: String
  , key :: String
  }

searchDropdownInputToHeaderSingleInput
  :: ∀ pq m
   . Warn (Text "This function is deprecated")
  => SearchDropdownInput
  -> Input pq Maybe (Object String) m
searchDropdownInputToHeaderSingleInput r =
  { items: Success r.items
  , insertable: NotInsertable
  , keepOpen: false
  , itemToObject: \a -> Object.singleton r.key (unsafePartial (fromJust (Object.lookup r.key a)))
  , debounceTime: Nothing
  , async: Nothing
  , render: renderHeaderSearchDropdown
      r.placeholder
      r.resetLabel
      renderLabel
      renderFuzzy
  }
  where
    renderFuzzy = span_ <<< boldMatches r.key

    renderLabel item =
      HH.text (fromMaybe "" $ Object.lookup r.key item)

searchDropdownInputToToolbarSingleInput
  :: ∀ pq m
   . Warn (Text "This function is deprecated")
  => SearchDropdownInput
  -> Input pq Maybe (Object String) m
searchDropdownInputToToolbarSingleInput r =
  { items: Success r.items
  , insertable: NotInsertable
  , keepOpen: false
  , itemToObject: \a -> Object.singleton r.key (unsafePartial (fromJust (Object.lookup r.key a)))
  , debounceTime: Nothing
  , async: Nothing
  , render: renderToolbarSearchDropdown
      r.placeholder
      r.resetLabel
      renderLabel
      renderFuzzy
  }
  where
    renderFuzzy = span_ <<< boldMatches r.key

    renderLabel item =
      HH.text (fromMaybe "" $ Object.lookup r.key item)


mountHeaderTypeahead :: EffectFn2 HTMLElement SearchDropdownInput (Interface MessageVariant QueryRow)
mountHeaderTypeahead = mkSingleTypeaheadMounter single' searchDropdownInputToHeaderSingleInput

mountToolbarTypeahead :: EffectFn2 HTMLElement SearchDropdownInput (Interface MessageVariant QueryRow)
mountToolbarTypeahead = mkSingleTypeaheadMounter single' searchDropdownInputToToolbarSingleInput
