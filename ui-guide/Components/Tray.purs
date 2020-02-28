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
data Action = Toggle

type Input = Unit

type Message = Void

component :: ∀ m . H.Component HH.HTML Query Input Message m
component =
  H.mkComponent
    { initialState: const { open: true }
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
    handleAction :: Action -> H.HalogenM State Action () Message m Unit
    handleAction = case _ of
      Toggle -> do
        state <- H.get
        H.modify_ _ { open = not state.open }

    render :: State -> H.ComponentHTML Action () m
    render state =
      HH.div_
      [ Documentation.customBlock_
        { header: "Tray"
        , subheader: "Interacting with Multiple Selected List Items"
        }
        [ Backdrop.backdrop_
          [ Button.button
            [ HE.onClick (const $ Just Toggle) ]
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
