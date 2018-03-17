module UIGuide.Components.Icon where

import Prelude

import Ocelot.Block.Icon as Icon
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

icon
  :: ∀ m
  . H.Component HH.HTML Query Input Message m
icon =
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
          { header: "Icon"
          , subheader: "Ocelot Font Icons"
          }
          [ Backdrop.backdrop_
            [ HH.div
              [ HP.class_ (HH.ClassName "flex justify-between space-between text-3xl w-full") ]
              [ Icon.back [ HP.class_ (HH.ClassName "text-blue-88") ]
              , Icon.caratDown_
              , Icon.caratRight_
              , Icon.caratUp_
              , Icon.close_
              , Icon.collapse_
              , Icon.dataSources_
              , Icon.download_
              , Icon.expand_
              , Icon.info_
              , Icon.menu_
              , Icon.options_
              , Icon.refresh_
              , Icon.search_
              , Icon.settings_
              , Icon.share_
              , Icon.timeline_
              ]
            ]
          ]
      ]
