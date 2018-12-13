module Ocelot.Interface.Dropdown where

import Prelude

import Control.Promise (Promise, fromAff)
import Data.Array (head)
import Data.Maybe (fromMaybe)
import Data.Symbol (SProxy(..))
import Data.Variant (Variant, inj)
import Effect.AVar (empty) as AVar
import Effect.Aff (launchAff_)
import Effect.Aff.AVar (put, read) as AffAVar
import Effect.Aff.Class (class MonadAff)
import Effect.Aff.Compat (EffectFn1, EffectFn2, mkEffectFn1, mkEffectFn2)
import Foreign.Object (Object, lookup)
import Halogen.VDom.Driver (runUI)
import Ocelot.Block.Button (button)
import Ocelot.Component.Dropdown (Input, Message(..), Query(..), component)
import Ocelot.Component.Dropdown.Render (defDropdown, render)
import Ocelot.Interface.Utilities (mkSubscription, Interface)
import Select (Visibility(..)) as Select
import Web.HTML (HTMLElement)

type QueryRow =
  ( setItems :: EffectFn1 (Array (Object String)) (Promise Unit)
  , setSelected :: EffectFn1 (Array (Object String)) (Promise Unit)
  )

type MessageVariant = Variant
  ( selected :: Object String
  , visibilityChanged :: Boolean
  , emit :: String
  )

convertMessageToVariant :: ∀ pq. Message pq (Object String) -> MessageVariant
convertMessageToVariant = case _ of
  Selected obj -> inj (SProxy :: SProxy "selected") obj
  VisibilityChanged vis -> inj (SProxy :: SProxy "visibilityChanged") (vis == Select.On)
  Emit _ -> inj (SProxy :: SProxy "emit") "emitted"

type ExternalInput =
  { selectedItem :: Array (Object String)
  , items :: Array (Object String)
  , placeholder :: String
  , key :: String
  }


externalInputToInput
  :: ∀ pq m
   . MonadAff m
  => ExternalInput
  -> Input pq (Object String) m
externalInputToInput { items, selectedItem, key, placeholder } =
  { items
  , selectedItem: head selectedItem
  , render: render $ defDropdown button [] (fromMaybe placeholder <<< (lookup key)) placeholder
  }

mountDropdown :: EffectFn2 HTMLElement ExternalInput (Interface MessageVariant QueryRow)
mountDropdown = mkEffectFn2 \el ext -> do
  ioVar <- AVar.empty
  launchAff_ do
    io <- runUI component (externalInputToInput ext) el
    AffAVar.put io ioVar
  pure
    { subscribe: mkSubscription ioVar convertMessageToVariant
    , setItems: mkEffectFn1 \arr -> fromAff do
       io <- AffAVar.read ioVar
       io.query $ SetItems arr unit
    , setSelected: mkEffectFn1 \arr -> fromAff do
       io <- AffAVar.read ioVar
       io.query $ SetSelection (head arr) unit
    }
