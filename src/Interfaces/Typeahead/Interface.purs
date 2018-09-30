-- | A JavaScript interface for the Ocelot typeahead, which
-- | only supports sync typeaheads.
module Ocelot.Interface.Typeahead where

import Prelude

import Control.Coroutine (consumer)
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Array (head, length)
import Data.Fuzzy (match)
import Data.Maybe (Maybe(..), maybe)
import Data.Symbol (SProxy(..))
import Data.Variant (Variant, inj)
import Effect (Effect)
import Effect.AVar as AVar
import Effect.Aff (Aff, error, killFiber, launchAff, launchAff_)
import Effect.Aff.AVar as AffAVar
import Effect.Aff.Compat (EffectFn1, EffectFn2, EffectFn3, mkEffectFn1, mkEffectFn2, mkEffectFn3, runEffectFn1)
import Effect.Class (liftEffect)
import Foreign.Object (Object)
import Halogen (HalogenIO)
import Halogen.HTML (span_)
import Halogen.HTML.Properties (placeholder) as HP
import Halogen.VDom.Driver (runUI)
import Network.RemoteData (RemoteData(..))
import Ocelot.Block.ItemContainer (boldMatches)
import Ocelot.Component.Typeahead (Input, Insertable(..), Message(..), Query(..), defRenderContainer, multi, renderMulti, renderSingle, single)
import Web.HTML (HTMLElement)

-- | A subset of the queries available to the typeahead, restricted
-- | to queries that would be sensible to trigger externally.
type QueryRow =
  ( remove :: EffectFn1 (Object String) (Promise Unit)
  , removeAll :: Effect (Promise Unit)
  , triggerFocus :: Effect (Promise Unit)
  , search :: EffectFn1 String (Promise Unit)
  , getSelected :: Effect (Promise (Array (Object String)))
  , replaceSelected :: EffectFn1 (Array (Object String)) (Promise Unit)
  , replaceItems :: EffectFn3 String String (Array (Object String)) (Promise Unit)
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
  SelectionChanged arr _ -> inj (SProxy :: SProxy "selectionChanged") arr
  Emit _ -> inj (SProxy :: SProxy "emit") "emitted"

convertSingleToMessageVariant :: ∀ pq. Message pq Maybe (Object String) -> MessageVariant
convertSingleToMessageVariant = case _ of
  Searched str -> inj (SProxy :: SProxy "searched") str
  Selected obj -> inj (SProxy :: SProxy "selected") obj
  SelectionChanged (Just i) _ -> inj (SProxy :: SProxy "selectionChanged") [i]
  SelectionChanged Nothing _ -> inj (SProxy :: SProxy "selectionChanged") []
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
    -- one of 'success', 'failure', 'loading', or 'notAsked'
  , status :: String
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
  { items: getStatus r.status "Failed to load some data." r.items
  , insertable: NotInsertable
  , keepOpen: r.keepOpen
  , itemToObject: \a -> a
  , asyncConfig: Nothing
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
  { items: getStatus r.status "Failed to load some data." r.items
  , insertable: NotInsertable
  , keepOpen: r.keepOpen
  , itemToObject: \a -> a
  , asyncConfig: Nothing
  , render: renderMulti
      [ HP.placeholder r.placeholder ]
      (renderFuzzy <<< match false identity "")
      (defRenderContainer renderFuzzy)
  }
  where
    renderFuzzy = span_ <<< boldMatches r.key


-- A helper function for parsing strings to status types
getStatus
  :: String
  -> String
  -> Array (Object String)
  -> RemoteData String (Array (Object String))
getStatus s e i
  | s == "success" || s == "Success" = Success i
  | s == "failure" || s == "Failure" = Failure e
  | s == "loading" || s == "Loading" = Loading
  | s == "notAsked" || s == "NotAsked" = NotAsked
  | otherwise = Success i

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
    , replaceSelected: mkEffectFn1 \arr -> Promise.fromAff do
        io <- AffAVar.read ioVar
        io.query $ ReplaceSelected arr unit
    , replaceItems: mkEffectFn3 \status err arr -> Promise.fromAff do
        io <- AffAVar.read ioVar
        io.query $ ReplaceItems (getStatus status err arr) unit
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
    , replaceSelected: mkEffectFn1 \arr -> Promise.fromAff do
        io <- AffAVar.read ioVar
        io.query $ ReplaceSelected (if length arr > 0 then head arr else Nothing) unit
    , replaceItems: mkEffectFn3 \status err arr -> Promise.fromAff do
        io <- AffAVar.read ioVar
        io.query $ ReplaceItems (getStatus status err arr) unit
    , reset: Promise.fromAff do
        io <- AffAVar.read ioVar
        io.query $ Reset unit
    }

----------
-- Utilities

-- | A row containing the 'subscribe' function from Halogen so it
-- | may be used to subscribe to the message variant in JS.
type WithHalogen out r =
  ( subscribe :: EffectFn1 (EffectFn1 out Unit) (Effect Unit) | r )

-- | A record containing the various queries available to be
-- | triggered in JS and the various messages that may be passed
-- | via the subscribe() function.
type Interface messages queries =
  { | WithHalogen messages queries }

-- | A helper to construct a coroutine for the `subscribe` function, allowing
-- | JS callers to consume a stream of outputs from the component. For now this
-- | is specialized to Aff.
mkSubscription
  :: ∀ f o o'
   . AffAVar.AVar (HalogenIO f o Aff)
  -> (o -> o')
  -> EffectFn1 (EffectFn1 o' Unit) (Effect Unit)
mkSubscription ioVar convertMessage = mkEffectFn1 \cb -> do
  fiber <- launchAff do
    io <- AffAVar.read ioVar
    io.subscribe $ consumer \msg -> do
      let msgVariant = convertMessage msg
      liftEffect $ runEffectFn1 cb msgVariant
      pure Nothing
  pure $ launchAff_ $ killFiber (error "unsubscribed") fiber
