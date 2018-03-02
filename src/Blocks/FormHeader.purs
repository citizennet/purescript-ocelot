module Ocelot.Block.FormHeader where

import Prelude

import Ocelot.Block.Button as Button
import Data.Maybe (Maybe)
import DOM.Event.Types (MouseEvent)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type FormHeaderProps i =
  { onClick :: (MouseEvent -> Maybe i)
  , name :: String
  , title :: String
  }

headerClasses :: Array HH.ClassName
headerClasses = HH.ClassName <$>
  [ "bg-black-10"
  , "flex"
  , "items-center"
  , "h-16"
  , "p-4"
  , "text-base"
  , "w-full"
  ]

formHeader :: ∀ p i. FormHeaderProps i -> HH.HTML p i
formHeader props =
  HH.header
    [ HP.classes headerClasses ]
    [ HH.div
        [ HP.class_ (HH.ClassName "flex-1") ]
        [ HH.span
            [ HP.class_ (HH.ClassName "text-grey-70 mr-4") ]
            [ HH.text props.name ]
        , HH.span
            [ HP.class_ (HH.ClassName "text-white") ]
            [ HH.text props.title ]
        ]
    , HH.span
        [ HP.class_( HH.ClassName "mr-2" ) ]
        [ Button.button
            { type_: Button.Transparent }
            []
            [ HH.text "Cancel" ]
        ]
    , Button.button
        { type_: Button.Primary }
        [ HE.onClick props.onClick ]
        [ HH.text "Create" ]
    ]
