-- | Not an entire component, but pieces that only make sense within
-- | the context of components that meet the constraints on these
-- | functions.
module Ocelot.Part.Modal where

import Prelude

import DOM.HTML.Indexed (HTMLdiv)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as ES
import Ocelot.Block.Format as Format
import Ocelot.HTML.Properties (css, (<&>))
import Web.Event.EventTarget as ET
import Web.HTML (window, HTMLDocument)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET

----------
-- Eval Partials

-- A function that will register an event source on the window
initializeWith
  :: ∀ s f g p o m
   . MonadAff m
  => (KE.KeyboardEvent -> (H.SubscribeStatus -> H.SubscribeStatus) -> f H.SubscribeStatus)
  -> H.HalogenM s f g p o m Unit
initializeWith query = do
  document <- H.liftEffect $ document =<< window
  H.subscribe
    $ ES.eventSource'
      (onKeyDown document)
      (Just <<< H.request <<< query)
  pure unit
  where
    onKeyDown
      :: HTMLDocument
      -> (KE.KeyboardEvent -> Effect Unit)
      -> Effect (Effect Unit)
    onKeyDown doc fn = do
      let target = HTMLDocument.toEventTarget doc
      listener <- ET.eventListener (traverse_ fn <<< KE.fromEvent)
      ET.addEventListener KET.keydown listener false target
      pure $ ET.removeEventListener KET.keydown listener false target

whenClose
  :: ∀ s f g p o m a
   . MonadAff m
  => KE.KeyboardEvent
  -> (H.SubscribeStatus -> a)
  -> H.HalogenM s f g p o m Unit
  -> H.HalogenM s f g p o m a
whenClose ev reply f = case KE.code ev of
  "Escape" -> f $> reply H.Done
  _ -> pure (reply H.Listening)


----------
-- Render Partials

-- Modals already come with the ClickOutside event baked in, while
-- end users are responsible for handling it somehow.
modal
  :: ∀ f g p m
   . (Unit -> f Unit)
  -> Array (HH.IProp HTMLdiv (f Unit))
  -> Array (H.ParentHTML f g p m)
  -> H.ParentHTML f g p m
modal click iprops html =
  HH.div_
    [ HH.div
        [ HP.classes backgroundClasses
        , HE.onClick $ HE.input_ click
        ]
        []
    , HH.div
        ( [ HP.classes modalClasses ] <&> iprops )
        html
    ]

modal_
  :: ∀ f g p m
   . (Unit -> f Unit)
  -> Array (H.ParentHTML f g p m)
  -> H.ParentHTML f g p m
modal_ query = modal query []


-----------
-- Blocks

type HeaderProps p i =
  { buttons :: Array (HH.HTML p i)
  , title :: Array (HH.HTML p i)
  }

backgroundClasses :: Array HH.ClassName
backgroundClasses = HH.ClassName <$>
  [ "fixed"
  , "pin"
  , "bg-black-modal-a90"
  , "fade-in"
  , "z-10"
  ]

modalClasses :: Array HH.ClassName
modalClasses = HH.ClassName <$>
  [ "fixed"
  , "pin-x"
  , "pin-t"
  , "my-20"
  , "m-auto"
  , "max-w-lg"
  , "slide-down"
  , "z-10"
  ]

bodyClasses :: Array HH.ClassName
bodyClasses = HH.ClassName <$>
  [ "relative"
  , "bg-grey-95"
  , "overflow-auto"
  , "max-h-full"
  , "w-full"
  , "flex-col"
  , "flex"
  , "rounded-b"
  ]

body
  :: ∀ p i
   . Array (HH.IProp HTMLdiv i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
body iprops html =
  HH.div
    ( [ HP.classes bodyClasses ] <&> iprops )
    html

body_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
body_ = body []

headerClasses :: Array HH.ClassName
headerClasses = HH.ClassName <$>
  [ "h-24"
  , "flex"
  ]

outerHeaderClasses :: Array HH.ClassName
outerHeaderClasses = HH.ClassName <$>
  [ "bg-white"
  , "w-full"
  , "px-6"
  , "items-center"
  , "flex"
  , "rounded-t"
  ]

innerHeaderClasses :: Array HH.ClassName
innerHeaderClasses = HH.ClassName <$>
  [ "w-full"
  , "items-center"
  , "mx-auto"
  , "flex"
  ]

header
  :: ∀ p i
   . HeaderProps p i
  -> HH.HTML p i
header props =
  HH.div
    [ HP.classes headerClasses ]
    [ HH.header
      [ HP.classes outerHeaderClasses ]
      ( [ HH.div
        [ HP.classes innerHeaderClasses ]
          [ Format.subHeading
            [ css "mb-0" ]
            props.title
          ]
        ]
        <> props.buttons
      )
    ]
