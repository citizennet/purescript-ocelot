module Ocelot.Block.Button where

import Prelude

import DOM.HTML.Indexed (HTMLbutton, HTMLdiv)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ocelot.HTML.Properties ((<&>))

buttonSharedClasses :: Array HH.ClassName
buttonSharedClasses = HH.ClassName <$>
  [ "no-outline"
  , "px-4"
  , "py-2"
  , "!active:border-b"
  , "active:border-t"
  , "disabled:opacity-50"
  , "disabled:cursor-default"
  , "!disabled:cursor-pointer"
  ]

buttonMainClasses :: Array HH.ClassName
buttonMainClasses = buttonSharedClasses <>
  ( HH.ClassName <$>
    [ "rounded"
    ]
  )

buttonClasses :: Array HH.ClassName
buttonClasses = HH.ClassName <$>
  [ "bg-grey-50-a20"
  , "border-grey-50-a20"
  , "hover:!disabled:bg-grey-50-a30"
  , "focus:bg-grey-50-a30"
  , "text-black-20"
  ]

buttonPrimaryClasses :: Array HH.ClassName
buttonPrimaryClasses = HH.ClassName <$>
  [ "bg-blue-88"
  , "border-blue-88"
  , "hover:!disabled:bg-blue-82"
  , "focus:bg-blue-82"
  , "text-white"
  ]

buttonDarkClasses :: Array HH.ClassName
buttonDarkClasses = HH.ClassName <$>
  [ "bg-grey-70-a30"
  , "border-grey-70-a30"
  , "hover:!disabled:bg-grey-70-a40"
  , "focus:bg-grey-70-a40"
  , "text-grey-70"
  ]

buttonClearClasses :: Array HH.ClassName
buttonClearClasses = HH.ClassName <$>
  [ "bg-transparent"
  , "border-transparent"
  , "text-grey-70"
  , "hover:text-grey-70-a30"
  , "focus:text-grey-70-a30"
  ]

buttonGroupClasses :: Array HH.ClassName
buttonGroupClasses = HH.ClassName <$>
  [ "flex"
  , "items-center"
  ]

centerClasses :: Array HH.ClassName
centerClasses = HH.ClassName <$>
  [ "mr-px"
  ]

leftClasses :: Array HH.ClassName
leftClasses = HH.ClassName <$>
  [ "mr-px"
  , "rounded-l"
  ]

rightClasses :: Array HH.ClassName
rightClasses = HH.ClassName <$>
  [ "rounded-r"
  ]

buttonBuilder
  :: ∀ p i
   . Array HH.ClassName
  -> Array (HH.IProp HTMLbutton i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
buttonBuilder classes iprops =
  HH.button
    ( [ HP.classes $ buttonMainClasses <> classes ] <&> iprops )

button
  :: ∀ p i
   . Array (HH.IProp HTMLbutton i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
button =
  buttonBuilder buttonClasses

button_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
button_ = button []

buttonPrimary
  :: ∀ p i
   . Array (HH.IProp HTMLbutton i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
buttonPrimary =
  buttonBuilder buttonPrimaryClasses

buttonPrimary_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
buttonPrimary_ = buttonPrimary []

buttonDark
  :: ∀ p i
   . Array (HH.IProp HTMLbutton i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
buttonDark =
  buttonBuilder buttonDarkClasses

buttonDark_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
buttonDark_ = buttonDark []

buttonClear
  :: ∀ p i
   . Array (HH.IProp HTMLbutton i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
buttonClear =
  buttonBuilder buttonClearClasses

buttonClear_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
buttonClear_ = buttonClear []

buttonGroup
  :: ∀ p i
   . Array (HH.IProp HTMLdiv i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
buttonGroup iprops =
  HH.div
    ( [ HP.classes buttonGroupClasses ] <&> iprops )

buttonGroup_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
buttonGroup_ = buttonGroup []

buttonGroupBuilder
  :: ∀ p i
   . Array HH.ClassName
  -> Array (HH.IProp HTMLbutton i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
buttonGroupBuilder classes iprops =
  HH.button
    ( [ HP.classes $ buttonSharedClasses <> classes ] <&> iprops )

buttonCenter
  :: ∀ p i
   . Array (HH.IProp HTMLbutton i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
buttonCenter =
  buttonGroupBuilder $ buttonClasses <> centerClasses

buttonCenter_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
buttonCenter_ = buttonCenter []

buttonPrimaryCenter
  :: ∀ p i
   . Array (HH.IProp HTMLbutton i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
buttonPrimaryCenter =
  buttonGroupBuilder $ buttonPrimaryClasses <> centerClasses

buttonPrimaryCenter_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
buttonPrimaryCenter_ = buttonPrimaryCenter []

buttonDarkCenter
  :: ∀ p i
   . Array (HH.IProp HTMLbutton i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
buttonDarkCenter =
  buttonGroupBuilder $ buttonDarkClasses <> centerClasses

buttonDarkCenter_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
buttonDarkCenter_ = buttonDarkCenter []

buttonClearCenter
  :: ∀ p i
   . Array (HH.IProp HTMLbutton i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
buttonClearCenter =
  buttonGroupBuilder $ buttonClearClasses <> centerClasses

buttonLeft
  :: ∀ p i
   . Array (HH.IProp HTMLbutton i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
buttonLeft =
  buttonGroupBuilder $ buttonClasses <> leftClasses

buttonLeft_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
buttonLeft_ = buttonLeft []

buttonPrimaryLeft
  :: ∀ p i
   . Array (HH.IProp HTMLbutton i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
buttonPrimaryLeft =
  buttonGroupBuilder $ buttonPrimaryClasses <> leftClasses

buttonPrimaryLeft_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
buttonPrimaryLeft_ = buttonPrimaryLeft []

buttonDarkLeft
  :: ∀ p i
   . Array (HH.IProp HTMLbutton i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
buttonDarkLeft =
  buttonGroupBuilder $ buttonDarkClasses <> leftClasses

buttonDarkLeft_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
buttonDarkLeft_ = buttonDarkLeft []

buttonClearLeft
  :: ∀ p i
   . Array (HH.IProp HTMLbutton i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
buttonClearLeft =
  buttonGroupBuilder $ buttonClearClasses <> leftClasses

buttonClearLeft_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
buttonClearLeft_ = buttonClearLeft []

buttonRight
  :: ∀ p i
   . Array (HH.IProp HTMLbutton i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
buttonRight =
  buttonGroupBuilder $ buttonClasses <> rightClasses

buttonRight_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
buttonRight_ = buttonRight []

buttonPrimaryRight
  :: ∀ p i
   . Array (HH.IProp HTMLbutton i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
buttonPrimaryRight =
  buttonGroupBuilder $ buttonPrimaryClasses <> rightClasses

buttonPrimaryRight_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
buttonPrimaryRight_ = buttonPrimaryRight []

buttonDarkRight
  :: ∀ p i
   . Array (HH.IProp HTMLbutton i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
buttonDarkRight =
  buttonGroupBuilder $ buttonDarkClasses <> rightClasses

buttonDarkRight_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
buttonDarkRight_ = buttonDarkRight []

buttonClearRight
  :: ∀ p i
   . Array (HH.IProp HTMLbutton i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
buttonClearRight =
  buttonGroupBuilder $ buttonClearClasses <> rightClasses

buttonClearRight_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
buttonClearRight_ = buttonClearRight []
