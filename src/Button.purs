module Ocelot.Button
  ( button
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
  , rightClasses
  )
  where

import Prelude

import DOM.HTML.Indexed as DOM.HTML.Indexed
import Halogen.HTML as Halogen.HTML
import Halogen.HTML.Properties as Halogen.HTML.Properties
import Ocelot.HTML.Properties ((<&>))

button
  :: ∀ p i
   . Array (Halogen.HTML.IProp DOM.HTML.Indexed.HTMLbutton i)
  -> Array (Halogen.HTML.HTML p i)
  -> Halogen.HTML.HTML p i
button = buttonBuilder buttonClasses

button_
  :: ∀ p i
   . Array (Halogen.HTML.HTML p i)
  -> Halogen.HTML.HTML p i
button_ = button []

buttonBuilder
  :: ∀ p i
   . Array Halogen.HTML.ClassName
  -> Array (Halogen.HTML.IProp DOM.HTML.Indexed.HTMLbutton i)
  -> Array (Halogen.HTML.HTML p i)
  -> Halogen.HTML.HTML p i
buttonBuilder classes iprops =
  Halogen.HTML.button
    ( [ Halogen.HTML.Properties.classes $ buttonMainClasses <> classes ] <&> iprops )

buttonCenter
  :: ∀ p i
   . Array (Halogen.HTML.IProp DOM.HTML.Indexed.HTMLbutton i)
  -> Array (Halogen.HTML.HTML p i)
  -> Halogen.HTML.HTML p i
buttonCenter =
  buttonGroupBuilder $ buttonClasses <> centerClasses

buttonCenter_
  :: ∀ p i
   . Array (Halogen.HTML.HTML p i)
  -> Halogen.HTML.HTML p i
buttonCenter_ = buttonCenter []

buttonClasses :: Array Halogen.HTML.ClassName
buttonClasses = Halogen.HTML.ClassName <$>
  [ "bg-grey-50-a20"
  , "border-grey-50-a20"
  , "hover:!disabled:bg-grey-50-a30"
  , "focus:bg-grey-50-a30"
  , "text-black-20"
  ]

buttonClear
  :: ∀ p i
   . Array (Halogen.HTML.IProp DOM.HTML.Indexed.HTMLbutton i)
  -> Array (Halogen.HTML.HTML p i)
  -> Halogen.HTML.HTML p i
buttonClear =
  buttonBuilder buttonClearClasses

buttonClear_
  :: ∀ p i
   . Array (Halogen.HTML.HTML p i)
  -> Halogen.HTML.HTML p i
buttonClear_ = buttonClear []

buttonClearCenter
  :: ∀ p i
   . Array (Halogen.HTML.IProp DOM.HTML.Indexed.HTMLbutton i)
  -> Array (Halogen.HTML.HTML p i)
  -> Halogen.HTML.HTML p i
buttonClearCenter =
  buttonGroupBuilder $ buttonClearClasses <> centerClasses

buttonClearCenter_
  :: ∀ p i
   . Array (Halogen.HTML.HTML p i)
  -> Halogen.HTML.HTML p i
buttonClearCenter_ = buttonClearCenter []

buttonClearClasses :: Array Halogen.HTML.ClassName
buttonClearClasses = Halogen.HTML.ClassName <$>
  [ "bg-transparent"
  , "border-transparent"
  , "text-grey-70"
  , "hover:text-grey-70-a30"
  , "focus:text-grey-70-a30"
  ]

buttonClearLeft
  :: ∀ p i
   . Array (Halogen.HTML.IProp DOM.HTML.Indexed.HTMLbutton i)
  -> Array (Halogen.HTML.HTML p i)
  -> Halogen.HTML.HTML p i
buttonClearLeft =
  buttonGroupBuilder $ buttonClearClasses <> leftClasses

buttonClearLeft_
  :: ∀ p i
   . Array (Halogen.HTML.HTML p i)
  -> Halogen.HTML.HTML p i
buttonClearLeft_ = buttonClearLeft []

buttonClearRight
  :: ∀ p i
   . Array (Halogen.HTML.IProp DOM.HTML.Indexed.HTMLbutton i)
  -> Array (Halogen.HTML.HTML p i)
  -> Halogen.HTML.HTML p i
buttonClearRight =
  buttonGroupBuilder $ buttonClearClasses <> rightClasses

buttonClearRight_
  :: ∀ p i
   . Array (Halogen.HTML.HTML p i)
  -> Halogen.HTML.HTML p i
buttonClearRight_ = buttonClearRight []

buttonDark
  :: ∀ p i
   . Array (Halogen.HTML.IProp DOM.HTML.Indexed.HTMLbutton i)
  -> Array (Halogen.HTML.HTML p i)
  -> Halogen.HTML.HTML p i
buttonDark =
  buttonBuilder buttonDarkClasses

buttonDark_
  :: ∀ p i
   . Array (Halogen.HTML.HTML p i)
  -> Halogen.HTML.HTML p i
buttonDark_ = buttonDark []

buttonDarkCenter
  :: ∀ p i
   . Array (Halogen.HTML.IProp DOM.HTML.Indexed.HTMLbutton i)
  -> Array (Halogen.HTML.HTML p i)
  -> Halogen.HTML.HTML p i
buttonDarkCenter =
  buttonGroupBuilder $ buttonDarkClasses <> centerClasses

buttonDarkCenter_
  :: ∀ p i
   . Array (Halogen.HTML.HTML p i)
  -> Halogen.HTML.HTML p i
buttonDarkCenter_ = buttonDarkCenter []

buttonDarkClasses :: Array Halogen.HTML.ClassName
buttonDarkClasses = Halogen.HTML.ClassName <$>
  [ "bg-grey-70-a30"
  , "border-grey-70-a30"
  , "hover:!disabled:bg-grey-70-a40"
  , "focus:bg-grey-70-a40"
  , "text-white"
  ]

buttonDarkLeft
  :: ∀ p i
   . Array (Halogen.HTML.IProp DOM.HTML.Indexed.HTMLbutton i)
  -> Array (Halogen.HTML.HTML p i)
  -> Halogen.HTML.HTML p i
buttonDarkLeft =
  buttonGroupBuilder $ buttonDarkClasses <> leftClasses

buttonDarkLeft_
  :: ∀ p i
   . Array (Halogen.HTML.HTML p i)
  -> Halogen.HTML.HTML p i
buttonDarkLeft_ = buttonDarkLeft []

buttonDarkRight
  :: ∀ p i
   . Array (Halogen.HTML.IProp DOM.HTML.Indexed.HTMLbutton i)
  -> Array (Halogen.HTML.HTML p i)
  -> Halogen.HTML.HTML p i
buttonDarkRight =
  buttonGroupBuilder $ buttonDarkClasses <> rightClasses

buttonDarkRight_
  :: ∀ p i
   . Array (Halogen.HTML.HTML p i)
  -> Halogen.HTML.HTML p i
buttonDarkRight_ = buttonDarkRight []

buttonGroup
  :: ∀ p i
   . Array (Halogen.HTML.IProp DOM.HTML.Indexed.HTMLdiv i)
  -> Array (Halogen.HTML.HTML p i)
  -> Halogen.HTML.HTML p i
buttonGroup iprops =
  Halogen.HTML.div
    ( [ Halogen.HTML.Properties.classes buttonGroupClasses ] <&> iprops )

buttonGroup_
  :: ∀ p i
   . Array (Halogen.HTML.HTML p i)
  -> Halogen.HTML.HTML p i
buttonGroup_ = buttonGroup []

buttonGroupBuilder
  :: ∀ p i
   . Array Halogen.HTML.ClassName
  -> Array (Halogen.HTML.IProp DOM.HTML.Indexed.HTMLbutton i)
  -> Array (Halogen.HTML.HTML p i)
  -> Halogen.HTML.HTML p i
buttonGroupBuilder classes iprops =
  Halogen.HTML.button
    ( [ Halogen.HTML.Properties.classes $ buttonSharedClasses <> classes ] <&> iprops )

buttonGroupClasses :: Array Halogen.HTML.ClassName
buttonGroupClasses = Halogen.HTML.ClassName <$>
  [ "flex"
  , "items-center"
  ]

buttonLeft
  :: ∀ p i
   . Array (Halogen.HTML.IProp DOM.HTML.Indexed.HTMLbutton i)
  -> Array (Halogen.HTML.HTML p i)
  -> Halogen.HTML.HTML p i
buttonLeft =
  buttonGroupBuilder $ buttonClasses <> leftClasses

buttonLeft_
  :: ∀ p i
   . Array (Halogen.HTML.HTML p i)
  -> Halogen.HTML.HTML p i
buttonLeft_ = buttonLeft []

buttonMainClasses :: Array Halogen.HTML.ClassName
buttonMainClasses = buttonSharedClasses <>
  ( Halogen.HTML.ClassName <$>
    [ "rounded"
    ]
  )

buttonPrimary
  :: ∀ p i
   . Array (Halogen.HTML.IProp DOM.HTML.Indexed.HTMLbutton i)
  -> Array (Halogen.HTML.HTML p i)
  -> Halogen.HTML.HTML p i
buttonPrimary =
  buttonBuilder buttonPrimaryClasses

buttonPrimary_
  :: ∀ p i
   . Array (Halogen.HTML.HTML p i)
  -> Halogen.HTML.HTML p i
buttonPrimary_ = buttonPrimary []

buttonPrimaryCenter
  :: ∀ p i
   . Array (Halogen.HTML.IProp DOM.HTML.Indexed.HTMLbutton i)
  -> Array (Halogen.HTML.HTML p i)
  -> Halogen.HTML.HTML p i
buttonPrimaryCenter =
  buttonGroupBuilder $ buttonPrimaryClasses <> centerClasses

buttonPrimaryCenter_
  :: ∀ p i
   . Array (Halogen.HTML.HTML p i)
  -> Halogen.HTML.HTML p i
buttonPrimaryCenter_ = buttonPrimaryCenter []

buttonPrimaryClasses :: Array Halogen.HTML.ClassName
buttonPrimaryClasses = Halogen.HTML.ClassName <$>
  [ "bg-blue-88"
  , "border-blue-88"
  , "hover:!disabled:bg-blue-82"
  , "focus:bg-blue-82"
  , "text-white"
  ]

buttonPrimaryLeft
  :: ∀ p i
   . Array (Halogen.HTML.IProp DOM.HTML.Indexed.HTMLbutton i)
  -> Array (Halogen.HTML.HTML p i)
  -> Halogen.HTML.HTML p i
buttonPrimaryLeft =
  buttonGroupBuilder $ buttonPrimaryClasses <> leftClasses

buttonPrimaryLeft_
  :: ∀ p i
   . Array (Halogen.HTML.HTML p i)
  -> Halogen.HTML.HTML p i
buttonPrimaryLeft_ = buttonPrimaryLeft []

buttonPrimaryRight
  :: ∀ p i
   . Array (Halogen.HTML.IProp DOM.HTML.Indexed.HTMLbutton i)
  -> Array (Halogen.HTML.HTML p i)
  -> Halogen.HTML.HTML p i
buttonPrimaryRight =
  buttonGroupBuilder $ buttonPrimaryClasses <> rightClasses

buttonPrimaryRight_
  :: ∀ p i
   . Array (Halogen.HTML.HTML p i)
  -> Halogen.HTML.HTML p i
buttonPrimaryRight_ = buttonPrimaryRight []

buttonRight
  :: ∀ p i
   . Array (Halogen.HTML.IProp DOM.HTML.Indexed.HTMLbutton i)
  -> Array (Halogen.HTML.HTML p i)
  -> Halogen.HTML.HTML p i
buttonRight =
  buttonGroupBuilder $ buttonClasses <> rightClasses

buttonRight_
  :: ∀ p i
   . Array (Halogen.HTML.HTML p i)
  -> Halogen.HTML.HTML p i
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
