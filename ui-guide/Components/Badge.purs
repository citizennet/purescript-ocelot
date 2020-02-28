module UIGuide.Component.Badge where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Ocelot.Block.Badge as Badge
import Ocelot.Block.Format as Format
import Ocelot.HTML.Properties (css)
import UIGuide.Block.Backdrop as Backdrop
import UIGuide.Block.Documentation as Documentation

type State = Unit

data Query a
type Action = Unit

type Input = Unit

type Message = Void


----------
-- HTML

component
  :: ∀ m. H.Component HH.HTML Query Input Message m
component
  = H.mkComponent
    { initialState: const unit
    , render
    , eval: H.mkEval H.defaultEval
    }
    where

      render :: State -> H.ComponentHTML Action () m
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
