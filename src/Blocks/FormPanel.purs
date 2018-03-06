module Ocelot.Block.FormPanel where

import Prelude

import DOM.HTML.Indexed (HTMLbutton)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type FormPanelProps =
  { isOpen :: Boolean
  , renderToggle :: Boolean -> HH.PlainHTML
  }

buttonClasses :: Array HH.ClassName
buttonClasses = HH.ClassName <$>
  [ "font-medium"
  , "no-outline"
  , "text-blue-82"
  ]

formPanel
  :: ∀ p i
   . FormPanelProps
  -> Array (HH.IProp HTMLbutton i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
formPanel props iprops html =
  HH.div
    [ HP.class_ (HH.ClassName "w-full") ]
    [ HH.div
        [ HP.classes contentClasses ]
        html
    , HH.button
        (iprops <> [ HP.classes buttonClasses ])
        [ (HH.fromPlainHTML (props.renderToggle props.isOpen)) ]
    ]
  where
    contentClasses =
      if props.isOpen
        then [ HH.ClassName "mb-6" ]
        else [ HH.ClassName "hidden" ]
