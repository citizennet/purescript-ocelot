module UIGuide.Components.DatePickers where

import Prelude

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Timer (TIMER)
import DOM (DOM)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax (AJAX)
import Ocelot.Block.Card as Card
import Ocelot.Block.Format as Format
import Ocelot.Components.DatePicker as DatePicker
import Ocelot.HTML.Properties (css)
import UIGuide.Block.Backdrop as Backdrop
import UIGuide.Block.Documentation as Documentation


----------
-- Component Types

type State = Unit

data Query a
  = NoOp a

----------
-- Child paths

type ChildSlot = Unit
type ChildQuery = DatePicker.Query

----------
-- Component definition

type Effects eff =
  ( avar :: AVAR
  , dom :: DOM
  , ajax :: AJAX
  , timer :: TIMER
  , now :: NOW
  , console :: CONSOLE
  | eff
  )

component :: ∀ eff m
  . MonadAff (Effects eff) m
 => H.Component HH.HTML Query Unit Void m
component =
  H.parentComponent
  { initialState: const unit
  , render
  , eval
  , receiver: const Nothing
  }
  where
    render
      :: State
      -> H.ParentHTML Query ChildQuery ChildSlot m
    render _ = cnDocumentationBlocks

    eval
      :: Query
      ~> H.ParentDSL State Query ChildQuery ChildSlot Void m
    eval (NoOp next) = pure next


----------
-- HTML

content :: ∀ p i. Array (HH.HTML p (i Unit)) -> HH.HTML p (i Unit)
content = Backdrop.content [ css "flex" ]

cnDocumentationBlocks :: ∀ eff m
  . MonadAff (Effects eff) m
 => H.ParentHTML Query ChildQuery ChildSlot m
cnDocumentationBlocks =
  HH.div_
    [ Documentation.block_
      { header: "Date Pickers - Single-Select"
      , subheader: "It's a date picker. Deal with it."
      }
      [ Backdrop.backdrop_
        [ content
          [ Card.card
            [ HP.class_ $ HH.ClassName "flex-1" ]
            [ Format.subHeading_ [ HH.text "Standard" ]
            , HH.slot unit DatePicker.component unit (const Nothing)
            ]
          ]
        ]
      ]
    ]
