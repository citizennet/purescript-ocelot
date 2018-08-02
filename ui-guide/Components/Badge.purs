module UIGuide.Component.Badge where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Ocelot.Block.Badge as Badge
import Ocelot.Block.Format as Format
import Ocelot.HTML.Properties (css)
import UIGuide.Block.Backdrop as Backdrop
import UIGuide.Block.Documentation as Documentation

type State = Unit

data Query a = NoOp a

type Input = Unit

type Message = Void


----------
-- HTML

component
  :: ∀ m. H.Component HH.HTML Query Input Message m
component
  = H.component
    { initialState: const unit
    , render
    , eval
    , receiver: const Nothing
    }
    where
      eval :: Query ~> H.ComponentDSL State Query Message m
      eval = case _ of
        NoOp a -> pure a

      render :: State -> H.ComponentHTML Query
      render _ =
        HH.div_
          [ Documentation.block_
            { header: "Badges"
            , subheader: "Badge all the things!"
            }
            [ Backdrop.backdrop_
              [ Backdrop.content_
                [ HH.div
                  [ css "flex-1 flex flex-col justify-between" ]
                  [ Format.p_
                    [ Badge.badgeSmall_ [ HH.text "1" ]
                    ]
                  , Format.p_
                    [ Badge.badge_ [ HH.text "2" ]
                    ]
                  , Format.p_
                    [ Badge.badgeLarge_ [ HH.text "3" ]
                    ]
                  ]
                ]
              ]
            , Backdrop.backdrop_
              [ Backdrop.content_
                [ HH.div
                  [ css "flex-1 flex flex-col justify-between" ]
                  [ row
                    [ HH.text "Leading text"
                    , Badge.badgeSmall
                      [ css "ml-1" ]
                      [ HH.text "1" ]
                    ]
                  , row
                    [ HH.text "Leading text"
                    , Badge.badge
                      [ css "ml-1" ]
                      [ HH.text "2" ]
                    ]
                  , row
                    [ HH.text "Leading text"
                    , Badge.badgeLarge
                      [ css "ml-1" ]
                      [ HH.text "3" ]
                    ]
                  ]
                ]
              ]
            , Backdrop.backdropDark_
              [ Backdrop.content_
                [ HH.div
                  [ css "flex-1 flex flex-col justify-between" ]
                  [ Format.p_
                    [ Badge.badgeSmall_ [ HH.text "1" ]
                    ]
                  , Format.p_
                    [ Badge.badge_ [ HH.text "2" ]
                    ]
                  , Format.p_
                    [ Badge.badgeLarge_ [ HH.text "3" ]
                    ]
                  ]
                ]
              ]
            , Backdrop.backdropDark_
              [ Backdrop.content_
                [ HH.div
                  [ css "flex-1 flex flex-col justify-between" ]
                  [ row
                    [ HH.text "Leading text"
                    , Badge.badgeSmall
                      [ css "ml-1" ]
                      [ HH.text "1" ]
                    ]
                  , row
                    [ HH.text "Leading text"
                    , Badge.badge
                      [ css "ml-1" ]
                      [ HH.text "2" ]
                    ]
                  , row
                    [ HH.text "Leading text"
                    , Badge.badgeLarge
                      [ css "ml-1" ]
                      [ HH.text "3" ]
                    ]
                  ]
                ]
              ]
            ]
          ]

      row :: ∀ p i. Array (HH.HTML p i) -> HH.HTML p i
      row = Format.p [ css "flex items-center" ]
