module CN.UI.Block.Button where

import Prelude

import DOM.HTML.Indexed (HTMLbutton)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

data ButtonType
  = Default
  | Primary 

data ButtonSize
  = Small
  | Medium
  | Large

type ButtonProps =
  { type_ :: ButtonType }

buttonClasses :: Array HH.ClassName
buttonClasses = 
  [ HH.ClassName "px-3 py-2 rounded shadow text-sm" ]

buttonTypeClasses :: ButtonType -> Array HH.ClassName
buttonTypeClasses Default =
  [ HH.ClassName "bg-grey-light hover:bg-grey" ]
buttonTypeClasses Primary =
  [ HH.ClassName "bg-teal hover:bg-teal-dark text-white" ]

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
