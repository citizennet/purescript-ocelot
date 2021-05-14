-- | Not an entire component, but pieces that only make sense within
-- | the context of components that meet the constraints on these
-- | functions.
module Ocelot.Part.Panel where

import Prelude

import DOM.HTML.Indexed (HTMLdiv)
import Data.Maybe (Maybe)
import Effect.Aff.Class (class MonadAff)
import Foreign.Object as Foreign.Object
import Halogen as Halogen
import Halogen.HTML as Halogen.HTML
import Halogen.HTML.Properties as Halogen.HTML.Properties
import Halogen.Query.Event as Halogen.Query.Event
import Ocelot.Block.Format as Ocelot.Block.Format
import Ocelot.HTML.Properties ((<&>))
import Ocelot.HTML.Properties as Ocelot.HTML.Properties
import Web.HTML as Web.HTML
import Web.HTML.HTMLDocument as Web.HTML.HTMLDocument
import Web.HTML.Window as Web.HTML.Window
import Web.UIEvent.KeyboardEvent as Web.UIEvent.KeyboardEvent
import Web.UIEvent.KeyboardEvent.EventTypes as Web.UIEvent.KeyboardEvent.EventTypes

----------
-- Eval Partials

-- A function that will register an event source on the window
initializeWith ::
  forall state action slots output m.
  MonadAff m =>
  (Web.UIEvent.KeyboardEvent.KeyboardEvent -> Maybe action) ->
  Halogen.HalogenM state action slots output m Halogen.SubscriptionId
initializeWith toAction = do
  document <- Halogen.liftEffect $ Web.HTML.Window.document =<< Web.HTML.window
  Halogen.subscribe
    $ Halogen.Query.Event.eventListener
      Web.UIEvent.KeyboardEvent.EventTypes.keydown
      (Web.HTML.HTMLDocument.toEventTarget document)
      (toAction <=< Web.UIEvent.KeyboardEvent.fromEvent)

whenClose ::
  forall state action slots output m.
  MonadAff m =>
  Web.UIEvent.KeyboardEvent.KeyboardEvent ->
  Halogen.SubscriptionId ->
  Halogen.HalogenM state action slots output m Unit ->
  Halogen.HalogenM state action slots output m Unit
whenClose ev sid close =
  when (Web.UIEvent.KeyboardEvent.code ev == "Escape") do
    Halogen.unsubscribe sid
    close

----------
-- Render Partials

-- Panels already come with the ClickOutside event baked in, while
-- end users are responsible for handling it somehow.
panelLeft ::
  forall action slots m.
  Boolean ->
  action ->
  Array (Halogen.HTML.IProp HTMLdiv action) ->
  Array (Halogen.ComponentHTML action slots m) ->
  Halogen.ComponentHTML action slots m
panelLeft visible click iprops html =
  Halogen.HTML.div
    (if visible then backgroundOpenProperties else backgroundClosedProperties)
    [ Halogen.HTML.div
        ( (if visible then panelLeftOpenProperties else panelLeftClosedProperties)
            <&> iprops
        )
        html
    ]

panelLeft_ ::
  forall action slots m.
  Boolean ->
  action ->
  Array (Halogen.ComponentHTML action slots m) ->
  Halogen.ComponentHTML action slots m
panelLeft_ visible query = panelLeft visible query []

panelRight ::
  forall action slots m.
  Boolean ->
  action ->
  Array (Halogen.HTML.IProp HTMLdiv action) ->
  Array (Halogen.ComponentHTML action slots m) ->
  Halogen.ComponentHTML action slots m
panelRight visible click iprops html =
  Halogen.HTML.div
    (if visible then backgroundOpenProperties else backgroundClosedProperties)
    [ Halogen.HTML.div
        ( (if visible then panelRightOpenProperties else panelRightClosedProperties)
            <&> iprops
        )
        html
    ]

panelRight_ ::
  forall action slots m.
  Boolean ->
  action ->
  Array (Halogen.ComponentHTML action slots m) ->
  Halogen.ComponentHTML action slots m
panelRight_ visible query = panelRight visible query []

-----------
-- Blocks

type HeaderProps p i =
  { buttons :: Array (Halogen.HTML.HTML p i)
  , title :: Array (Halogen.HTML.HTML p i)
  }

backgroundOpenProperties :: forall i. Array (Halogen.HTML.IProp HTMLdiv i)
backgroundOpenProperties =
  [ Halogen.HTML.Properties.classes (backgroundClasses <> backgroundOpenClasses) ]

backgroundClosedProperties :: forall i. Array (Halogen.HTML.IProp HTMLdiv i)
backgroundClosedProperties =
  [ Halogen.HTML.Properties.classes (backgroundClasses <> backgroundClosedClasses) ]

panelLeftOpenProperties :: forall i. Array (Halogen.HTML.IProp HTMLdiv i)
panelLeftOpenProperties =
  [ Halogen.HTML.Properties.classes (panelClasses <> panelLeftClasses)
  ]

panelRightOpenProperties :: forall i. Array (Halogen.HTML.IProp HTMLdiv i)
panelRightOpenProperties =
  [ Halogen.HTML.Properties.classes (panelClasses <> panelRightClasses)
  ]

panelLeftClosedProperties :: forall i. Array (Halogen.HTML.IProp HTMLdiv i)
panelLeftClosedProperties =
  [ Halogen.HTML.Properties.classes panelClasses
  , Ocelot.HTML.Properties.style panelLeftCss
  ]

panelRightClosedProperties :: forall i. Array (Halogen.HTML.IProp HTMLdiv i)
panelRightClosedProperties =
  [ Halogen.HTML.Properties.classes panelClasses
  , Ocelot.HTML.Properties.style panelRightCss
  ]

backgroundClasses :: Array Halogen.HTML.ClassName
backgroundClasses =
  [ "bg-black-modal-a90"
  , "fade-in"
  , "fixed"
  , "pin"
  , "z-10"
  ]
    <#> Halogen.HTML.ClassName

backgroundOpenClasses :: Array Halogen.HTML.ClassName
backgroundOpenClasses =
  [ "opacity-1"
  , "visible"
  ]
    <#> Halogen.HTML.ClassName

backgroundClosedClasses :: Array Halogen.HTML.ClassName
backgroundClosedClasses =
  [ "invisible"
  , "opacity-0"
  ]
    <#> Halogen.HTML.ClassName

panelClasses :: Array Halogen.HTML.ClassName
panelClasses =
  [ "absolute"
  , "h-full"
  , "overflow-y-auto"
  , "transition-1/2-out"
  , "z-10"
  ]
    <#> Halogen.HTML.ClassName

panelLeftClasses :: Array Halogen.HTML.ClassName
panelLeftClasses =
  [ "pin-l"
  ]
    <#> Halogen.HTML.ClassName

panelLeftCss :: Foreign.Object.Object String
panelLeftCss =
  Foreign.Object.fromHomogeneous
    { transform: "translate(-300px)"
    }

panelRightClasses :: Array Halogen.HTML.ClassName
panelRightClasses =
  [ "pin-r"
  ]
    <#> Halogen.HTML.ClassName

panelRightCss :: Foreign.Object.Object String
panelRightCss =
  Foreign.Object.fromHomogeneous
    { transform: "translate(300px)"
    }

bodyClasses :: Array Halogen.HTML.ClassName
bodyClasses =
  [ "bg-grey-95"
  , "flex"
  , "flex-col"
  , "rounded-b"
  , "w-full"
  ]
    <#> Halogen.HTML.ClassName

body ::
  forall p i.
  Array (Halogen.HTML.IProp HTMLdiv i) ->
  Array (Halogen.HTML.HTML p i) ->
  Halogen.HTML.HTML p i
body iprops html =
  Halogen.HTML.div
    ( [ Halogen.HTML.Properties.classes bodyClasses ] <&> iprops )
    html

body_ ::
  forall p i.
  Array (Halogen.HTML.HTML p i) ->
  Halogen.HTML.HTML p i
body_ = body []

headerClasses :: Array Halogen.HTML.ClassName
headerClasses =
  [ "h-24"
  , "flex"
  ]
    <#> Halogen.HTML.ClassName

outerHeaderClasses :: Array Halogen.HTML.ClassName
outerHeaderClasses =
  [ "bg-white"
  , "w-full"
  , "px-6"
  , "items-center"
  , "flex"
  , "rounded-t"
  ]
    <#> Halogen.HTML.ClassName

innerHeaderClasses :: Array Halogen.HTML.ClassName
innerHeaderClasses =
  [ "w-full"
  , "items-center"
  , "mx-auto"
  , "flex"
  ]
    <#> Halogen.HTML.ClassName

header ::
  forall p i.
  HeaderProps p i ->
  Halogen.HTML.HTML p i
header props =
  Halogen.HTML.div
    [ Halogen.HTML.Properties.classes headerClasses ]
    [ Halogen.HTML.header
      [ Halogen.HTML.Properties.classes outerHeaderClasses ]
      ( [ Halogen.HTML.div
        [ Halogen.HTML.Properties.classes innerHeaderClasses ]
          [ Ocelot.Block.Format.subHeading
            [ Ocelot.HTML.Properties.css "mb-0" ]
            props.title
          ]
        ]
        <> props.buttons
      )
    ]
