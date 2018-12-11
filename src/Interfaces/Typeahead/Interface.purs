-- | A JavaScript interface for the Ocelot typeahead, which
-- | only supports sync typeaheads.
module Ocelot.Interface.Typeahead where

import Prelude

import Control.Coroutine (consumer)
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Array (head)
import Data.Fuzzy (match)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Symbol (SProxy(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Variant (Variant, inj)
import Effect (Effect)
import Effect.AVar as AVar
import Effect.Aff (Aff, error, killFiber, launchAff, launchAff_)
import Effect.Aff.AVar as AffAVar
import Effect.Aff.Compat (EffectFn1, EffectFn2, mkEffectFn1, mkEffectFn2, runEffectFn1)
import Effect.Class (liftEffect)
import Foreign.Object (Object)
import Foreign.Object as Object
import Halogen (HalogenIO)
import Halogen.HTML (span_)
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Network.RemoteData (RemoteData(..))
import Ocelot.Block.ItemContainer (boldMatches)
import Ocelot.Component.Typeahead (Input, Insertable(..), Message(..), Query(..), defRenderContainer, multi, renderMulti, renderSingle, single)
import Ocelot.Interface.Utilities (mkSubscription, WithHalogen, Interface)
import Partial.Unsafe (unsafePartial)
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
-- | - status: a status to put the typeahead in: one of 'success', 'loading'
-- |   'failure', or 'notAsked'
-- | - placeholder: placeholder text to set in the field
-- | - key: the name of the field in the object that should be displayed in the list
-- | - keepOpen: whether the typeahead should stay open or close on selection
type ExternalInput =
  { items :: Array (Object String)
  , debounceTime :: Int
  , placeholder :: String
  , key :: String
  , keepOpen :: Boolean
  }

-- | An adapter to simplify types necessary in JS to control the typeahead
-- | component. Items are specialized to Object String.
externalInputToSingleInput
  :: ∀ pq m
   . ExternalInput
  -> Input pq Maybe (Object String) m
externalInputToSingleInput r =
  { items: Success r.items
  , insertable: NotInsertable
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

externalInputToMultiInput
  :: ∀ pq m
   . ExternalInput
  -> Input pq Array (Object String) m
externalInputToMultiInput r =
  { items: Success r.items
  , insertable: NotInsertable
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

mountMultiTypeahead :: EffectFn2 HTMLElement ExternalInput (Interface MessageVariant QueryRow)
mountMultiTypeahead = mkEffectFn2 \el ext -> do
  ioVar <- AVar.empty
  launchAff_ do
    io <- runUI multi (externalInputToMultiInput ext) el
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


mountSingleTypeahead :: EffectFn2 HTMLElement ExternalInput (Interface MessageVariant QueryRow)
mountSingleTypeahead = mkEffectFn2 \el ext -> do
  ioVar <- AVar.empty
  launchAff_ do
    io <- runUI single (externalInputToSingleInput ext) el
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

