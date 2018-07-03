module UIGuide.Components.Table where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ocelot.Blocks.Table as Table
import Ocelot.HTML.Properties (css)
import UIGuide.Block.Backdrop as Backdrop
import UIGuide.Block.Documentation as Documentation

type State = Unit

data Query a = NoOp a

type Input = Unit

type Message = Void

component
  :: ∀ m
   . H.Component HH.HTML Query Input Message m
component =
  H.component
    { initialState: const unit
    , render
    , eval
    , receiver: const Nothing
    }
  where
    eval
      :: Query
      ~> H.ComponentDSL State Query Message m
    eval = case _ of
      NoOp a -> do
        pure a

    render
      :: State
      -> H.ComponentHTML Query
    render _ =
      HH.div_
        [ Documentation.block_
            { header: "Table"
            , subheader: "Tabular Data"
            }
            [ Backdrop.backdrop_
              [ renderTable ]
            ]
        ]
      where
        renderTable =
          HH.table
            [ css "w-100" ]
            [ Table.thead_ 
              [ HH.tr_ 
                [ Table.th_
                  [ HH.text "Name" ]
                ]
              ]
            , Table.tbody_
              [ HH.tr_
                [ Table.td_
                  [ HH.text "Dave's OO Patterns" ]
                ]
              ]
            ]
