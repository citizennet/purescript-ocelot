module UIGuide.Component.Diagram where

import Prelude

import Halogen as Halogen
import Halogen.HTML as Halogen.HTML
import Ocelot.Block.Card as Card
import Ocelot.Block.Diagram as Ocelot.Block.Diagram
import Ocelot.HTML.Properties as Ocelot.HTML.Properties
import UIGuide.Block.Backdrop as Backdrop
import UIGuide.Block.Documentation as Documentation

type Component m = Halogen.Component Halogen.HTML.HTML Query Input Output m

type ComponentHTML m = Halogen.ComponentHTML Action ChildSlots m

type ComponentM m a = Halogen.HalogenM State Action ChildSlots Output m a

type State = Unit

data Query a

type Action = Unit

type Input = Unit

type Output = Void

type ChildSlots =
  ()

----------
-- HTML

component :: forall m. Component m
component =
  Halogen.mkComponent
    { initialState: const unit
    , render
    , eval: Halogen.mkEval Halogen.defaultEval
    }

render :: forall m. State -> ComponentHTML m
render _ =
  Halogen.HTML.div_
  [ Documentation.block_
    { header: "Percentage Bar Graph"
    , subheader: "Visualize a number in percentage"
    }
    [ Backdrop.backdrop_
      [ Backdrop.content_
        [ Card.card_
          [ Halogen.HTML.p
            [ Ocelot.HTML.Properties.css "flex items-center" ]
            [ Halogen.HTML.span
              [ Ocelot.HTML.Properties.css "min-w-5"]
              [ Halogen.HTML.text "A:" ]
            , Ocelot.Block.Diagram.percentBar percentA
              [ Ocelot.HTML.Properties.css "h-4 pl-2 w-1/3" ]
              [ Ocelot.HTML.Properties.css "bg-red"]
            , Halogen.HTML.span
              [ Ocelot.HTML.Properties.css "ml-4 text-red" ]
              [ Halogen.HTML.text (show percentA <> "%")]
            ]
          , Halogen.HTML.p
            [ Ocelot.HTML.Properties.css "flex items-center" ]
            [ Halogen.HTML.span
              [ Ocelot.HTML.Properties.css "min-w-5"]
              [ Halogen.HTML.text "B:" ]
            , Ocelot.Block.Diagram.percentBar percentB
              [ Ocelot.HTML.Properties.css "h-4 pl-2 w-1/3" ]
              [ Ocelot.HTML.Properties.css "bg-blue-82"]
            , Halogen.HTML.span
              [ Ocelot.HTML.Properties.css "ml-4 text-blue-82" ]
              [ Halogen.HTML.text (show percentB <> "%")]
            ]
          ]
        ]
      ]
    ]
  ]
  where
  percentA :: Number
  percentA = 30.0

  percentB :: Number
  percentB = 100.0
