module UIGuide.Components.Button where

import Prelude

import Ocelot.Block.Button as Button
import Ocelot.Block.Type as Type
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import UIGuide.Block.Backdrop as Backdrop
import UIGuide.Block.Documentation as Documentation

type State = Unit

data Query a = NoOp a

type Input = Unit

type Message = Void

component :: ∀ m. H.Component HH.HTML Query Input Message m
component =
  H.component
    { initialState: const unit
    , render
    , eval
    , receiver: const Nothing
    }
  where
    eval :: Query ~> H.ComponentDSL State Query Message m
    eval = case _ of
      NoOp a -> do
        pure a

    render :: State -> H.ComponentHTML Query
    render _ =
      HH.div_
      [ Documentation.documentation_
          { header: "Buttons"
          , subheader: "Perform Actions"
          }
          [ Backdrop.backdrop_
              [ HH.h3
                [ HP.classes Type.captionClasses ]
                [ HH.text "Standard Button" ]
              , Button.button_
                  { type_: Button.Default }
                  [ HH.text "Cancel" ]
              ]
          , Backdrop.backdrop_
              [ HH.h3
                [ HP.classes Type.captionClasses ]
                [ HH.text "Primary Button" ]
              , Button.button_
                  { type_: Button.Primary }
                  [ HH.text "Submit" ]
              ]
          , Backdrop.backdrop_
              [ HH.h3
                [ HP.classes Type.captionClasses ]
                [ HH.text "Standard Button with Dark Backdrop" ]
              , HH.div
                  [ HP.class_ (HH.ClassName "bg-black-10 flex items-center justify-center h-16 w-full") ]
                  [ Button.button_
                      { type_: Button.Secondary }
                      [ HH.text "Options" ]
                  ]
              ]
          ]
      ]
