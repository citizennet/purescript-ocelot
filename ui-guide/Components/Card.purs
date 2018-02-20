module UIGuide.Components.Card where

import Prelude

import CN.UI.Block.Card as Card
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import UIGuide.Block.Component as Component
import UIGuide.Block.Documentation as Documentation

type State = Unit

data Query a = NoOp a

type Input = Unit

type Message = Void

card 
  :: ∀ m
  . H.Component HH.HTML Query Input Message m
card =
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
      [ Documentation.documentation
          { header: "Card"
          , subheader: "Information on a card"
          }
          [ Component.component
              { title: "Card" }
                [ Card.card
                    { title: "Summary" }
                    meta
                ]
          ]
      ]
      where
        meta =
          [ HH.table_ 
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
          ] 