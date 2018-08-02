module UIGuide.Component.Tray where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Ocelot.Block.Badge as Badge
import Ocelot.Block.Button as Button
import Ocelot.Block.Tray as Tray
import Ocelot.HTML.Properties (css)
import UIGuide.Block.Backdrop as Backdrop
import UIGuide.Block.Documentation as Documentation

type State = { open :: Boolean }

data Query a
  = NoOp a
  | Toggle a

type Input = Unit

type Message = Void

component :: ∀ m . H.Component HH.HTML Query Input Message m
component =
  H.component
    { initialState: const { open: true }
    , render
    , eval
    , receiver: const Nothing
    }
  where
    eval :: Query ~> H.ComponentDSL State Query Message m
    eval = case _ of
      NoOp a -> pure a

      Toggle a -> do
        state <- H.get
        H.modify_ _ { open = not state.open }
        pure a

    render :: State -> H.ComponentHTML Query
    render state =
      HH.div_
      [ Documentation.customBlock_
        { header: "Tray"
        , subheader: "Interacting with Multiple Selected List Items"
        }
        [ Backdrop.backdrop_
          [ Button.button
            [ HE.onClick (HE.input_ Toggle) ]
            [ HH.text "toggle tray" ]
          , Tray.tray
            [ Tray.open state.open ]
            [ Badge.badgeLarge
              [ css "mr-2" ]
              [ HH.text "2" ]
            , HH.p
              [ css "mr-10" ]
              [ HH.text "Ads selected:" ]
            , Button.button_
              [ HH.text "View Ads" ]
            , Button.button
              [ css "ml-4" ]
              [ HH.text "Dave's Design Patterns" ]
            ]
          ]
        ]
      ]
