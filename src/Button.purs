module Ocelot.Button
  ( Decorator(..)
  , PillStyle(..)
  , button
  , buttonBuilder
  , buttonCenter
  , buttonCenter_
  , buttonClasses
  , buttonClear
  , buttonClearCenter
  , buttonClearCenter_
  , buttonClearClasses
  , buttonClearLeft
  , buttonClearLeft_
  , buttonClearRight
  , buttonClearRight_
  , buttonClear_
  , buttonDark
  , buttonDarkCenter
  , buttonDarkCenter_
  , buttonDarkClasses
  , buttonDarkLeft
  , buttonDarkLeft_
  , buttonDarkRight
  , buttonDarkRight_
  , buttonDark_
  , buttonGroup
  , buttonGroupBuilder
  , buttonGroupClasses
  , buttonGroup_
  , buttonLeft
  , buttonLeft_
  , buttonMainClasses
  , buttonPrimary
  , buttonPrimaryCenter
  , buttonPrimaryCenter_
  , buttonPrimaryClasses
  , buttonPrimaryLeft
  , buttonPrimaryLeft_
  , buttonPrimaryRight
  , buttonPrimaryRight_
  , buttonPrimary_
  , buttonRight
  , buttonRight_
  , buttonSharedClasses
  , button_
  , centerClasses
  , leftClasses
  , pill
  , pillAnchor
  , pillAnchor_
  , pillButton
  , pillButton_
  , pillClasses
  , pillPrimaryClasses
  , pill_
  , rightClasses
  ) where

import Prelude

import DOM.HTML.Indexed as DOM.HTML.Indexed
import Data.Array as Data.Array
import Data.Maybe (Maybe(..))
import Halogen.HTML as Halogen.HTML
import Halogen.HTML.Elements as Halogen.HTML.Elements
import Halogen.HTML.Properties as Halogen.HTML.Properties
import Ocelot.HTML.Properties ((<&>))
import Ocelot.HTML.Properties as Ocelot.HTML.Properties
import Option as Option

data Decorator i p
  = Left (Halogen.HTML.HTML p i)
  | Right (Halogen.HTML.HTML p i)

data PillStyle
  = Default
  | Primary

type IProp r i = Halogen.HTML.IProp (class :: String | r) i

button ::
  forall p i.
  Array (Halogen.HTML.IProp DOM.HTML.Indexed.HTMLbutton i) ->
  Array (Halogen.HTML.HTML p i) ->
  Halogen.HTML.HTML p i
button = buttonBuilder buttonClasses

button_ ::
  forall p i.
  Array (Halogen.HTML.HTML p i) ->
  Halogen.HTML.HTML p i
button_ = button []

buttonBuilder ::
  forall p i.
  Array Halogen.HTML.ClassName ->
  Array (Halogen.HTML.IProp DOM.HTML.Indexed.HTMLbutton i) ->
  Array (Halogen.HTML.HTML p i) ->
  Halogen.HTML.HTML p i
buttonBuilder classes iprops =
  Halogen.HTML.button
    ([ Halogen.HTML.Properties.classes $ buttonMainClasses <> classes ] <&> iprops)

buttonCenter ::
  forall p i.
  Array (Halogen.HTML.IProp DOM.HTML.Indexed.HTMLbutton i) ->
  Array (Halogen.HTML.HTML p i) ->
  Halogen.HTML.HTML p i
buttonCenter =
  buttonGroupBuilder $ buttonClasses <> centerClasses

buttonCenter_ ::
  forall p i.
  Array (Halogen.HTML.HTML p i) ->
  Halogen.HTML.HTML p i
buttonCenter_ = buttonCenter []

buttonClasses :: Array Halogen.HTML.ClassName
buttonClasses = Halogen.HTML.ClassName <$>
  [ "bg-grey-50-a20"
  , "border-grey-50-a20"
  , "hover:!disabled:bg-grey-50-a30"
  , "focus:bg-grey-50-a30"
  , "text-black-20"
  ]

buttonClear ::
  forall p i.
  Array (Halogen.HTML.IProp DOM.HTML.Indexed.HTMLbutton i) ->
  Array (Halogen.HTML.HTML p i) ->
  Halogen.HTML.HTML p i
buttonClear =
  buttonBuilder buttonClearClasses

buttonClear_ ::
  forall p i.
  Array (Halogen.HTML.HTML p i) ->
  Halogen.HTML.HTML p i
buttonClear_ = buttonClear []

buttonClearCenter ::
  forall p i.
  Array (Halogen.HTML.IProp DOM.HTML.Indexed.HTMLbutton i) ->
  Array (Halogen.HTML.HTML p i) ->
  Halogen.HTML.HTML p i
buttonClearCenter =
  buttonGroupBuilder $ buttonClearClasses <> centerClasses

buttonClearCenter_ ::
  forall p i.
  Array (Halogen.HTML.HTML p i) ->
  Halogen.HTML.HTML p i
buttonClearCenter_ = buttonClearCenter []

buttonClearClasses :: Array Halogen.HTML.ClassName
buttonClearClasses = Halogen.HTML.ClassName <$>
  [ "bg-transparent"
  , "border-transparent"
  , "text-grey-70"
  , "hover:text-grey-70-a30"
  , "focus:text-grey-70-a30"
  ]

buttonClearLeft ::
  forall p i.
  Array (Halogen.HTML.IProp DOM.HTML.Indexed.HTMLbutton i) ->
  Array (Halogen.HTML.HTML p i) ->
  Halogen.HTML.HTML p i
buttonClearLeft =
  buttonGroupBuilder $ buttonClearClasses <> leftClasses

buttonClearLeft_ ::
  forall p i.
  Array (Halogen.HTML.HTML p i) ->
  Halogen.HTML.HTML p i
buttonClearLeft_ = buttonClearLeft []

buttonClearRight ::
  forall p i.
  Array (Halogen.HTML.IProp DOM.HTML.Indexed.HTMLbutton i) ->
  Array (Halogen.HTML.HTML p i) ->
  Halogen.HTML.HTML p i
buttonClearRight =
  buttonGroupBuilder $ buttonClearClasses <> rightClasses

buttonClearRight_ ::
  forall p i.
  Array (Halogen.HTML.HTML p i) ->
  Halogen.HTML.HTML p i
buttonClearRight_ = buttonClearRight []

buttonDark ::
  forall p i.
  Array (Halogen.HTML.IProp DOM.HTML.Indexed.HTMLbutton i) ->
  Array (Halogen.HTML.HTML p i) ->
  Halogen.HTML.HTML p i
buttonDark =
  buttonBuilder buttonDarkClasses

buttonDark_ ::
  forall p i.
  Array (Halogen.HTML.HTML p i) ->
  Halogen.HTML.HTML p i
buttonDark_ = buttonDark []

buttonDarkCenter ::
  forall p i.
  Array (Halogen.HTML.IProp DOM.HTML.Indexed.HTMLbutton i) ->
  Array (Halogen.HTML.HTML p i) ->
  Halogen.HTML.HTML p i
buttonDarkCenter =
  buttonGroupBuilder $ buttonDarkClasses <> centerClasses

buttonDarkCenter_ ::
  forall p i.
  Array (Halogen.HTML.HTML p i) ->
  Halogen.HTML.HTML p i
buttonDarkCenter_ = buttonDarkCenter []

buttonDarkClasses :: Array Halogen.HTML.ClassName
buttonDarkClasses = Halogen.HTML.ClassName <$>
  [ "bg-grey-70-a30"
  , "border-grey-70-a30"
  , "hover:!disabled:bg-grey-70-a40"
  , "focus:bg-grey-70-a40"
  , "text-white"
  ]

buttonDarkLeft ::
  forall p i.
  Array (Halogen.HTML.IProp DOM.HTML.Indexed.HTMLbutton i) ->
  Array (Halogen.HTML.HTML p i) ->
  Halogen.HTML.HTML p i
buttonDarkLeft =
  buttonGroupBuilder $ buttonDarkClasses <> leftClasses

buttonDarkLeft_ ::
  forall p i.
  Array (Halogen.HTML.HTML p i) ->
  Halogen.HTML.HTML p i
buttonDarkLeft_ = buttonDarkLeft []

buttonDarkRight ::
  forall p i.
  Array (Halogen.HTML.IProp DOM.HTML.Indexed.HTMLbutton i) ->
  Array (Halogen.HTML.HTML p i) ->
  Halogen.HTML.HTML p i
buttonDarkRight =
  buttonGroupBuilder $ buttonDarkClasses <> rightClasses

buttonDarkRight_ ::
  forall p i.
  Array (Halogen.HTML.HTML p i) ->
  Halogen.HTML.HTML p i
buttonDarkRight_ = buttonDarkRight []

buttonGroup ::
  forall p i.
  Array (Halogen.HTML.IProp DOM.HTML.Indexed.HTMLdiv i) ->
  Array (Halogen.HTML.HTML p i) ->
  Halogen.HTML.HTML p i
buttonGroup iprops =
  Halogen.HTML.div
    ([ Halogen.HTML.Properties.classes buttonGroupClasses ] <&> iprops)

buttonGroup_ ::
  forall p i.
  Array (Halogen.HTML.HTML p i) ->
  Halogen.HTML.HTML p i
buttonGroup_ = buttonGroup []

buttonGroupBuilder ::
  forall p i.
  Array Halogen.HTML.ClassName ->
  Array (Halogen.HTML.IProp DOM.HTML.Indexed.HTMLbutton i) ->
  Array (Halogen.HTML.HTML p i) ->
  Halogen.HTML.HTML p i
buttonGroupBuilder classes iprops =
  Halogen.HTML.button
    ([ Halogen.HTML.Properties.classes $ buttonSharedClasses <> classes ] <&> iprops)

buttonGroupClasses :: Array Halogen.HTML.ClassName
buttonGroupClasses = Halogen.HTML.ClassName <$>
  [ "flex"
  , "items-center"
  ]

buttonLeft ::
  forall p i.
  Array (Halogen.HTML.IProp DOM.HTML.Indexed.HTMLbutton i) ->
  Array (Halogen.HTML.HTML p i) ->
  Halogen.HTML.HTML p i
buttonLeft =
  buttonGroupBuilder $ buttonClasses <> leftClasses

buttonLeft_ ::
  forall p i.
  Array (Halogen.HTML.HTML p i) ->
  Halogen.HTML.HTML p i
buttonLeft_ = buttonLeft []

buttonMainClasses :: Array Halogen.HTML.ClassName
buttonMainClasses = buttonSharedClasses <>
  ( Halogen.HTML.ClassName <$>
      [ "rounded"
      ]
  )

buttonPrimary ::
  forall p i.
  Array (Halogen.HTML.IProp DOM.HTML.Indexed.HTMLbutton i) ->
  Array (Halogen.HTML.HTML p i) ->
  Halogen.HTML.HTML p i
buttonPrimary =
  buttonBuilder buttonPrimaryClasses

buttonPrimary_ ::
  forall p i.
  Array (Halogen.HTML.HTML p i) ->
  Halogen.HTML.HTML p i
buttonPrimary_ = buttonPrimary []

buttonPrimaryCenter ::
  forall p i.
  Array (Halogen.HTML.IProp DOM.HTML.Indexed.HTMLbutton i) ->
  Array (Halogen.HTML.HTML p i) ->
  Halogen.HTML.HTML p i
buttonPrimaryCenter =
  buttonGroupBuilder $ buttonPrimaryClasses <> centerClasses

buttonPrimaryCenter_ ::
  forall p i.
  Array (Halogen.HTML.HTML p i) ->
  Halogen.HTML.HTML p i
buttonPrimaryCenter_ = buttonPrimaryCenter []

buttonPrimaryClasses :: Array Halogen.HTML.ClassName
buttonPrimaryClasses = Halogen.HTML.ClassName <$>
  [ "bg-blue-88"
  , "border-blue-88"
  , "hover:!disabled:bg-blue-82"
  , "focus:bg-blue-82"
  , "text-white"
  ]

buttonPrimaryLeft ::
  forall p i.
  Array (Halogen.HTML.IProp DOM.HTML.Indexed.HTMLbutton i) ->
  Array (Halogen.HTML.HTML p i) ->
  Halogen.HTML.HTML p i
buttonPrimaryLeft =
  buttonGroupBuilder $ buttonPrimaryClasses <> leftClasses

buttonPrimaryLeft_ ::
  forall p i.
  Array (Halogen.HTML.HTML p i) ->
  Halogen.HTML.HTML p i
buttonPrimaryLeft_ = buttonPrimaryLeft []

buttonPrimaryRight ::
  forall p i.
  Array (Halogen.HTML.IProp DOM.HTML.Indexed.HTMLbutton i) ->
  Array (Halogen.HTML.HTML p i) ->
  Halogen.HTML.HTML p i
buttonPrimaryRight =
  buttonGroupBuilder $ buttonPrimaryClasses <> rightClasses

buttonPrimaryRight_ ::
  forall p i.
  Array (Halogen.HTML.HTML p i) ->
  Halogen.HTML.HTML p i
buttonPrimaryRight_ = buttonPrimaryRight []

buttonRight ::
  forall p i.
  Array (Halogen.HTML.IProp DOM.HTML.Indexed.HTMLbutton i) ->
  Array (Halogen.HTML.HTML p i) ->
  Halogen.HTML.HTML p i
buttonRight =
  buttonGroupBuilder $ buttonClasses <> rightClasses

buttonRight_ ::
  forall p i.
  Array (Halogen.HTML.HTML p i) ->
  Halogen.HTML.HTML p i
buttonRight_ = buttonRight []

buttonSharedClasses :: Array Halogen.HTML.ClassName
buttonSharedClasses = Halogen.HTML.ClassName <$>
  [ "no-outline"
  , "px-4"
  , "py-2"
  , "!active:border-b"
  , "active:border-t"
  , "disabled:opacity-50"
  , "disabled:cursor-default"
  , "!disabled:cursor-pointer"
  ]

centerClasses :: Array Halogen.HTML.ClassName
centerClasses = Halogen.HTML.ClassName <$>
  [ "mr-px"
  ]

leftClasses :: Array Halogen.HTML.ClassName
leftClasses = Halogen.HTML.ClassName <$>
  [ "mr-px"
  , "rounded-l"
  ]

rightClasses :: Array Halogen.HTML.ClassName
rightClasses = Halogen.HTML.ClassName <$>
  [ "rounded-r"
  ]

pill ::
  forall i options r p.
  Option.FromRecord
    options
    ()
    ( decorator :: Decorator i p
    , props :: Array (IProp r i)
    , style :: PillStyle
    ) =>
  Halogen.HTML.Elements.Node (class :: String | r) p i ->
  Record options ->
  Array (Halogen.HTML.HTML p i) ->
  Halogen.HTML.HTML p i
pill elem optionsRecord html =
  elem ([ Halogen.HTML.Properties.classes classes ] <&> props)
    case options.decorator of
      Nothing -> html
      Just (Left decorator) ->
        Data.Array.cons
          ( Halogen.HTML.span
              [ Ocelot.HTML.Properties.css "pr-2" ]
              [ decorator ]
          )
          html
      Just (Right decorator) ->
        Data.Array.snoc
          html
          ( Halogen.HTML.span
              [ Ocelot.HTML.Properties.css "pl-2" ]
              [ decorator ]
          )
  where
  classes :: Array Halogen.HTML.ClassName
  classes = pillSharedClasses <> case options.style of
    Nothing -> pillClasses
    Just style -> case style of
      Default -> pillClasses
      Primary -> pillPrimaryClasses

  options ::
    { decorator :: Maybe (Decorator i p)
    , props :: Maybe (Array (IProp r i))
    , style :: Maybe PillStyle
    }
  options = Option.toRecord optionsOption

  optionsOption ::
    Option.Option
      ( decorator :: Decorator i p
      , props :: Array (IProp r i)
      , style :: PillStyle
      )
  optionsOption = Option.fromRecord optionsRecord

  props :: Array (IProp r i)
  props = case options.props of
    Nothing -> []
    Just props' -> props'

pill_ ::
  forall i r p.
  Halogen.HTML.Elements.Node (class :: String | r) p i ->
  Array (Halogen.HTML.HTML p i) ->
  Halogen.HTML.HTML p i
pill_ elem = pill elem {}

pillAnchor ::
  forall i options p.
  Option.FromRecord
    options
    ()
    ( decorator :: Decorator i p
    , props :: Array (Halogen.HTML.IProp DOM.HTML.Indexed.HTMLa i)
    , style :: PillStyle
    ) =>
  Record options ->
  Array (Halogen.HTML.HTML p i) ->
  Halogen.HTML.HTML p i
pillAnchor = pill Halogen.HTML.a

pillAnchor_ ::
  forall i p.
  Array (Halogen.HTML.HTML p i) ->
  Halogen.HTML.HTML p i
pillAnchor_ = pillAnchor {}

pillButton ::
  forall i options p.
  Option.FromRecord
    options
    ()
    ( decorator :: Decorator i p
    , props :: Array (Halogen.HTML.IProp DOM.HTML.Indexed.HTMLbutton i)
    , style :: PillStyle
    ) =>
  Record options ->
  Array (Halogen.HTML.HTML p i) ->
  Halogen.HTML.HTML p i
pillButton = pill Halogen.HTML.button

pillButton_ ::
  forall i p.
  Array (Halogen.HTML.HTML p i) ->
  Halogen.HTML.HTML p i
pillButton_ = pillButton {}

pillClasses :: Array Halogen.HTML.ClassName
pillClasses = Halogen.HTML.ClassName <$>
  [ "bg-grey-97"
  , "border"
  , "border-grey-70"
  , "flex"
  , "items-center"
  ]

pillPrimaryClasses :: Array Halogen.HTML.ClassName
pillPrimaryClasses = Halogen.HTML.ClassName <$>
  [ "bg-blue-88-a40"
  , "border"
  , "border-blue-75"
  ]

pillSharedClasses :: Array Halogen.HTML.ClassName
pillSharedClasses = Halogen.HTML.ClassName <$>
  [ "!active:border-b-2"
  , "active:border-t-2"
  , "disabled:opacity-50"
  , "disabled:cursor-default"
  , "!disabled:cursor-pointer"
  , "inline-block"
  , "no-outline"
  , "px-4"
  , "py-2"
  , "rounded"
  ]
