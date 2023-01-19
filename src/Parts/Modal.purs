-- | Not an entire component, but pieces that only make sense within
-- | the context of components that meet the constraints on these
-- | functions.
module Ocelot.Part.Modal where

import Prelude

import DOM.HTML.Indexed (HTMLdiv)
import Data.Maybe (Maybe)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Query.Event as ES
import Ocelot.Block.Format as Format
import Ocelot.HTML.Properties (css, (<&>))
import Web.HTML (window)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET

----------
-- Eval Partials

-- A function that will register an event source on the window
initializeWith ::
  forall state action slots output m.
  MonadAff m =>
  (KE.KeyboardEvent -> Maybe action) ->
  H.HalogenM state action slots output m H.SubscriptionId
initializeWith toAction = do
  document <- H.liftEffect $ document =<< window
  H.subscribe
    $ ES.eventListener
        KET.keydown
        (HTMLDocument.toEventTarget document)
        (toAction <=< KE.fromEvent)

whenClose ::
  forall state action slots output m.
  MonadAff m =>
  KE.KeyboardEvent ->
  H.SubscriptionId ->
  H.HalogenM state action slots output m Unit ->
  H.HalogenM state action slots output m Unit
whenClose ev sid close =
  when (KE.code ev == "Escape") do
    H.unsubscribe sid
    close

----------
-- Render Partials

-- Modals already come with the ClickOutside event baked in, while
-- end users are responsible for handling it somehow.
modal ::
  forall action slots m.
  action ->
  Array (HH.IProp HTMLdiv action) ->
  Array (H.ComponentHTML action slots m) ->
  H.ComponentHTML action slots m
modal _ iprops html =
  HH.div
    [ HP.classes backgroundClasses ]
    [ HH.div
        ([ HP.classes modalClasses ] <&> iprops)
        html
    ]

modal_ ::
  forall action slots m.
  action ->
  Array (H.ComponentHTML action slots m) ->
  H.ComponentHTML action slots m
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
  , "overflow-y-auto"
  ]

modalClasses :: Array HH.ClassName
modalClasses = HH.ClassName <$>
  [ "absolute"
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

body ::
  forall p i.
  Array (HH.IProp HTMLdiv i) ->
  Array (HH.HTML p i) ->
  HH.HTML p i
body iprops html =
  HH.div
    ([ HP.classes bodyClasses ] <&> iprops)
    html

body_ ::
  forall p i.
  Array (HH.HTML p i) ->
  HH.HTML p i
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

header ::
  forall p i.
  HeaderProps p i ->
  HH.HTML p i
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
