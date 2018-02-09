module CN.UI.Block.Button where

import Prelude

import DOM.HTML.Indexed (HTMLbutton)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

data ButtonType
  = Default
  | Primary
  | Secondary
  | Transparent 

type ButtonProps =
  { type_ :: ButtonType }

buttonClasses :: Array HH.ClassName
buttonClasses = HH.ClassName <$>
  [ "no-outline"
  , "px-3"
  , "py-2"
  , "rounded"
  , "shadow"
  , "text-sm"
  ]

buttonTypeClasses :: ButtonType -> Array HH.ClassName
buttonTypeClasses type_ = HH.ClassName <$> classNames type_
  where
    classNames Default = 
      [ "bg-grey-50-a20"
      , "hover:bg-grey-50-a30"
      , "text-black-20"
      ]

    classNames Primary =
      [ "bg-blue-88"
      , "hover:bg-blue-88"
      , "text-white" 
      ]

    classNames Secondary =
      [ "bg-grey-70-a30"
      , "hover:grey-70-a40"
      , "text-white"
      ]

    classNames Transparent =
      [ "bg-transparent"
      , "text-black-20"
      ]

button_
  :: ∀ p i
   . ButtonProps
  -> Array (HH.HTML p i)
  -> HH.HTML p i
button_ { type_ } =
  HH.button
    [ HP.classes (buttonTypeClasses type_ <> buttonClasses) ]

button
  :: ∀ p i
   . ButtonProps
  -> Array (HH.IProp HTMLbutton i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
button { type_ } iprops =
  HH.button
    ( iprops <> [ HP.classes (buttonTypeClasses type_ <> buttonClasses) ] )
