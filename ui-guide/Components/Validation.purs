module UIGuide.Components.Validation where

import Prelude

import CN.UI.Block.Button as Button
import CN.UI.Components.Dropdown as Dropdown
import CN.UI.Components.Typeahead as TA
import CN.UI.Core.Typeahead as TA

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Aff.Console (log, logShow, CONSOLE)
import Control.Monad.Eff.Timer (TIMER)

import DOM (DOM)
import DOM.Event.Types (MouseEvent)

import Data.Either.Nested (Either1, Either3)
import Data.Functor.Coproduct.Nested (Coproduct1, Coproduct3)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)

import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Network.HTTP.Affjax (AJAX)

import UIGuide.Utilities.Async as Async
import UIGuide.Block.Component as Component
import UIGuide.Block.Documentation as Documentation

----------
-- Component Types

type State
  = Unit

data Query a
  = NoOp a
  | HandleA (TA.TypeaheadMessage Query TestRecord Void Void) a
  | HandleB (TA.TypeaheadMessage Query Async.Todo (Async.Source Async.Todo) Async.Err) a
  | HandleC (TA.TypeaheadMessage Query Async.User (Async.Source Async.User) Async.Err) a


----------
-- Child paths

type ChildSlot = Either3 Unit Unit Unit
type ChildQuery eff m = Coproduct3
  (TA.TypeaheadQuery Query TestRecord Void Void eff m)
  (TA.TypeaheadQuery Query Async.Todo (Async.Source Async.Todo) Async.Err eff m)
  (TA.TypeaheadQuery Query Async.User (Async.Source Async.User) Async.Err eff m)


----------
-- Component definition

-- NOTE: Uses the same effects but does not compose with typeahead effects. Written out again from scratch.
type Effects eff =
  ( avar :: AVAR
  , dom :: DOM
  , ajax :: AJAX
  , timer :: TIMER
  , console :: CONSOLE | eff )

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
    -- For the sake of testing and visual demonstration, we'll just render
    -- out a bunch of selection variants in respective slots
    render
      :: State
      -> H.ParentHTML Query (ChildQuery (Effects eff) m) ChildSlot m
    render st = renderPage

    eval
      :: Query
      ~> H.ParentDSL State Query (ChildQuery (Effects eff) m) ChildSlot Void m
    eval (NoOp next) = pure next

    -- Done asynchronously so data can load in the background.
    eval (HandleA message next) = case message of
      _ -> pure next

    eval (HandleB message next) = case message of
      TA.RequestData syncMethod -> do
        _ <- H.fork do
          res <- H.liftAff $ Async.load syncMethod
          H.query' CP.cp2 unit
            $ H.action
            $ TA.FulfillRequest
            -- $ replaceItems res syncMethod
            $ syncMethod
        pure next
      _ -> pure next

    eval (HandleC message next) = case message of
      TA.RequestData syncMethod -> do
        _ <- H.fork do
          res <- H.liftAff $ Async.load syncMethod
          H.query' CP.cp3 unit
            $ H.action
            $ TA.FulfillRequest
            -- $ replaceItems res syncMethod
            $ syncMethod
        pure next

      _ -> pure next


----------
-- Rendering

renderPage =
  HH.div_
  [ Documentation.documentation
      { header: "Example Form"
      , subheader: "Test validations and form submission."
      }
      [ Component.component
          { title: "Text Field" }
          [ Button.button_
              { type_: Button.Default }
              [ HH.text "Cancel" ]
          ]
      , Component.component
          { title: "Dropdown" }
          [ Button.button_
              { type_: Button.Primary }
              [ HH.text "Submit" ]
          ]
      , Component.component
          { title: "Typeahead (Async)" }
          [ Button.button_
              { type_: Button.Primary }
              [ HH.text "Submit" ]
          ]
      ]
  ]


----------
-- Sample data

newtype TestRecord = TestRecord
  { name :: String
  , id :: Int
  }

instance eqTestRecord :: Eq TestRecord where
  eq (TestRecord { id: id'' }) (TestRecord { id: id' }) = id'' == id'

derive instance newtypeTestRecord :: Newtype TestRecord _

testRecords :: Array TestRecord
testRecords =
  [ TestRecord { name: "Chris", id: 0 }
  , TestRecord { name: "Dave", id: 1 }
  , TestRecord { name: "Thomas", id: 2 }
  , TestRecord { name: "Forest", id: 3 }
  ]
