module UIGuide.Component.Icons where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ocelot.Block.Format as Format
import Ocelot.Block.Icon as Icon
import Ocelot.HTML.Properties (css, (<&>))
import UIGuide.Block.Backdrop as Backdrop
import UIGuide.Block.Documentation as Documentation

type State = Unit

data Query a = NoOp a

type Input = Unit

type Message = Void

component :: ∀ m . H.Component HH.HTML Query Input Message m
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
      NoOp a -> pure a

    renderIcon :: ∀ p i. Tuple String (HH.HTML p i) -> HH.HTML p i
    renderIcon (Tuple name icon) =
      HH.div
        [ HP.class_ $ HH.ClassName "mx-6" ]
        [ Documentation.callout_
          [ Backdrop.backdrop_
            [ Backdrop.content_
              [ HH.p
                ( [ HP.classes Format.subHeadingClasses ] <&>
                  [ HP.class_ $ HH.ClassName "min-w-9 justify-center" ]
                )
                [ icon ]
              ]
            ]
          ]
        , Format.caption
          [ HP.class_ $ HH.ClassName "text-center" ]
          [ HH.text name ]
        ]

    render :: State -> H.ComponentHTML Query
    render _ =
      HH.div_
      [ Documentation.customBlock_
        { header: "Icon"
        , subheader: "Represent actions."
        }

        [ HH.div
          [ css "flex flex-wrap -mx-6 justify-start" ]
          ( renderIcon <$>
            [ Tuple "Settings" Icon.settings_
            , Tuple "Share" Icon.share_
            , Tuple "Download" Icon.download_
            , Tuple "Search" Icon.search_
            , Tuple "Refresh" Icon.refresh_
            , Tuple "Back" Icon.back_
            , Tuple "Menu" Icon.menu_
            , Tuple "Options" Icon.options_
            , Tuple "Close" Icon.close_
            , Tuple "Plus" Icon.plus_
            , Tuple "Delete" Icon.delete_
            , Tuple "Delete Circle" Icon.deleteCircle_
            , Tuple "Add" Icon.add_
            , Tuple "Added" $ Icon.added [ css "text-blue-88" ]
            , Tuple "Arrow Up" Icon.arrowUp_
            , Tuple "Arrow Down" Icon.arrowDown_
            , Tuple "Arrow Left" Icon.arrowLeft_
            , Tuple "Arrow Right" Icon.arrowRight_
            , Tuple "Collapse" Icon.collapse_
            , Tuple "Expand" Icon.expand_
            , Tuple "Chevron Left" Icon.chevronLeft_
            , Tuple "Chevron Right" Icon.chevronRight_
            , Tuple "Carat Up" Icon.caratUp_
            , Tuple "Carat Down" Icon.caratDown_
            , Tuple "Carat Left" Icon.caratLeft_
            , Tuple "Carat Right" Icon.caratRight_
            , Tuple "Error" $ Icon.error [ css "text-red" ]
            , Tuple "Tip" $ Icon.tip [ css "text-yellow" ]
            , Tuple "Info" $ Icon.info [ css "text-blue" ]
            , Tuple "Success" $ Icon.success [ css "text-green" ]
            , Tuple "Facebook" $ Icon.facebook [ css "text-fb-blue" ]
            , Tuple "Instagram" $ Icon.instagram [ css "text-ig-brown" ]
            , Tuple "Twitter" $ Icon.twitter [ css "text-tw-blue" ]
            , Tuple "Timeline" Icon.timeline_
            , Tuple "Navigate" Icon.navigate_
            , Tuple "Data Sources" Icon.dataSources_
            ]
          )
        ]
      ]
