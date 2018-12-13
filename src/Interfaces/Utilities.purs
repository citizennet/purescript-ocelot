module Ocelot.Interface.Utilities where

import Prelude

import Control.Coroutine (consumer)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, error, killFiber, launchAff, launchAff_)
import Effect.Aff.AVar (AVar, read) as AffAVar
import Effect.Aff.Compat (EffectFn1, mkEffectFn1, runEffectFn1)
import Effect.Class (liftEffect)
import Halogen (HalogenIO)

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
