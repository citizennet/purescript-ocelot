module UIGuide.Component.Card where

import Prelude

import Ocelot.Block.Card as Card
import Ocelot.Block.Format as Format
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import UIGuide.Block.Backdrop as Backdrop
import UIGuide.Block.Documentation as Documentation

type State = Unit

data Query a
type Action = Unit

type Input = Unit

type Message = Void

card
  :: ∀ m
  . H.Component HH.HTML Query Input Message m
card =
  H.mkComponent
    { initialState: const unit
    , render
    , eval: H.mkEval H.defaultEval
    }
  where

    render :: State -> H.ComponentHTML Action () m
    render _ =
      HH.div_
      [ Documentation.block_
          { header: "Card"
          , subheader: "Information on a card"
          }
          [ Backdrop.backdrop_
            [ Card.card_
              [ Format.contentHeading_
                [ HH.text "Summary" ]
              , meta
              ]
            ]
          ]
      ]
      where
        meta =
          HH.table_
            [ HH.tr_
              [ HH.th
                [ HP.class_ (HH.ClassName "font-medium text-left text-xs text-grey-50 pr-4 py-1") ]
                [ HH.text "Run Ads On:"]
              , HH.td
                [ HP.class_ (HH.ClassName "font-light text-left text-xs text-black-20 py-1") ]
                [ HH.text "Stack Overflow"]
              ]
            , HH.tr_
              [ HH.th
                  [ HP.class_ (HH.ClassName "font-medium text-left text-xs text-grey-50 pr-4 py-1") ]
                  [ HH.text "Social Account:"]
                , HH.td
                  [ HP.class_ (HH.ClassName "font-light text-left text-xs text-black-20 py-1") ]
                  [ HH.text "Dave Loves Gang of Four"]
                ]
            , HH.tr_
              [ HH.th
                [ HP.class_ (HH.ClassName "font-medium text-left text-xs text-grey-50 pr-4 py-1") ]
                [ HH.text "Ads Account:"]
              , HH.td
                [ HP.class_ (HH.ClassName "font-light text-left text-xs text-black-20 py-1") ]
                [ HH.text "123991234"]
              ]
            ]
