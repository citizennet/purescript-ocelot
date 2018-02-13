module UIGuide.Components.Validation where

import Prelude

import CN.UI.Block.Button as Button
import CN.UI.Block.FormControl as FormControl
import CN.UI.Block.Input as Input
import CN.UI.Components.Typeahead as TAInput
import CN.UI.Core.Typeahead as TA

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff.Timer (TIMER)

import DOM (DOM)

import Data.Either.Nested (Either4)
import Data.Functor.Coproduct.Nested (Coproduct4)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.Array (length)

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

type State =
  { developers :: Array TestRecord
  , todos :: Array Async.Todo
  , users1 :: Array Async.User
  , users2 :: Array Async.User }

data Query a
  = NoOp a
  | HandleA (TA.TypeaheadMessage Query TestRecord Void Void) a
  | HandleB (TA.TypeaheadMessage Query Async.Todo (Async.Source Async.Todo) Async.Err) a
  | HandleC (TA.TypeaheadMessage Query Async.User (Async.Source Async.User) Async.Err) a
  | HandleD (TA.TypeaheadMessage Query Async.User (Async.Source Async.User) Async.Err) a
  | FormSubmit a


----------
-- Child paths

type ChildSlot = Either4 Unit Unit Unit Unit
type ChildQuery eff m = Coproduct4
  (TA.TypeaheadQuery Query TestRecord Void Void eff m)
  (TA.TypeaheadQuery Query Async.Todo (Async.Source Async.Todo) Async.Err eff m)
  (TA.TypeaheadQuery Query Async.User (Async.Source Async.User) Async.Err eff m)
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
  { initialState: const { developers: [], todos: [], users1: [], users2: [] }
  , render
  , eval
  , receiver: const Nothing
  }
  where
    render
      :: State
      -> H.ParentHTML Query (ChildQuery (Effects eff) m) ChildSlot m
    render st = HH.div_
      ( [ renderForm ]
        <> ( if length st.developers > 0
              then [ HH.text (show st.developers) ]
              else [] )
        <> ( if length st.todos > 0
              then [ HH.text (show st.todos) ]
              else [] )
        <> ( if length st.users1 > 0
              then [ HH.text (show st.users1) ]
              else [] )
        <> ( if length st.users2 > 0
              then [ HH.text (show st.users2) ]
              else [] )
      )
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
          case TA.maybeReplaceItems res syncMethod of
            Nothing -> pure Nothing
            (Just newSync) -> do
               _ <- H.query' CP.cp2 unit $ H.action $ TA.FulfillRequest newSync
               pure Nothing
         pure next
      _ -> pure next

    eval (HandleC message next) = case message of
      TA.RequestData syncMethod -> do
         _ <- H.fork do
          res <- H.liftAff $ Async.load syncMethod
          case TA.maybeReplaceItems res syncMethod of
            Nothing -> pure Nothing
            (Just newSync) -> do
               _ <- H.query' CP.cp3 unit $ H.action $ TA.FulfillRequest newSync
               pure Nothing
         pure next
      _ -> pure next

    eval (HandleD message next) = case message of
      TA.RequestData syncMethod -> do
         _ <- H.fork do
          res <- H.liftAff $ Async.load syncMethod
          case TA.maybeReplaceItems res syncMethod of
            Nothing -> pure Nothing
            (Just newSync) -> do
               _ <- H.query' CP.cp4 unit $ H.action $ TA.FulfillRequest newSync
               pure Nothing
         pure next
      _ -> pure next

    eval (FormSubmit next) = do
      devs <- H.query' CP.cp1 unit (H.request TA.Selections)
      todos <- H.query' CP.cp2 unit (H.request TA.Selections)
      users1 <- H.query' CP.cp3 unit (H.request TA.Selections)
      users2 <- H.query' CP.cp4 unit (H.request TA.Selections)

      H.modify (_ { developers = fromMaybe [] $ TA.unpackSelections <$> devs
                  , todos = fromMaybe [] $ TA.unpackSelections <$> todos
                  , users1 = fromMaybe [] $ TA.unpackSelections <$> users1
                  , users2 = fromMaybe [] $ TA.unpackSelections <$> users2 })

      st <- H.get
      pure next



----------
-- Rendering

renderForm :: ∀ eff m
  . MonadAff (Effects eff) m
 => H.ParentHTML Query (ChildQuery (Effects eff) m) ChildSlot m
renderForm =
  HH.form
  [ HE.onSubmit $ HE.input_ FormSubmit ]
  [ Documentation.documentation
      { header: "Example Form"
      , subheader: "Test validations and form submission."
      }
      [ Component.component
        { title: "Typeaheads" }
        [ FormControl.formControl
          { label: "Developers"
          , helpText: Just "There are lots of developers to choose from."
          }
          ( HH.slot' CP.cp1 unit TA.component (TAInput.defaultMulti' testRecords) (HE.input HandleA) )
        , FormControl.formControl
          { label: "Todos"
          , helpText: Just "Synchronous todo fetching like you've always wanted."
          }
          ( HH.slot' CP.cp2 unit TA.component (TAInput.defaultAsyncMulti' Async.todos) (HE.input HandleB) )
        , FormControl.formControl
          { label: "Users"
          , helpText: Just "Oh, you REALLY need async, huh."
          }
          ( HH.slot' CP.cp3 unit TA.component (TAInput.defaultContAsyncMulti' Async.users) (HE.input HandleC) )
        , FormControl.formControl
          { label: "Users 2"
          , helpText: Just "Honestly, this is just lazy."
          }
          ( HH.slot' CP.cp4 unit TA.component (TAInput.defaultAsyncMulti' Async.users) (HE.input HandleD) )
        , FormControl.formControl
          { label: "Email"
          , helpText: Just "Dave will spam your email with gang of four patterns"
          }
          ( Input.input [ HP.placeholder "davelovesdesignpatterns@gmail.com" ] )
        , FormControl.formControl
          { label: "Username"
          , helpText: Just "Put your name in and we'll spam you forever"
          }
          ( Input.input [ HP.placeholder "Placehold me" ] )
        , Button.button
            { type_: Button.Primary }
            [ HP.type_ HP.ButtonSubmit ]
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

instance show :: Show TestRecord where
  show (TestRecord { name }) = name

instance compareToStringTestRecord :: TA.CompareToString TestRecord where
  compareToString (TestRecord { name }) = name

derive instance newtypeTestRecord :: Newtype TestRecord _

testRecords :: Array TestRecord
testRecords =
  [ TestRecord { name: "Chris", id: 0 }
  , TestRecord { name: "Dave", id: 1 }
  , TestRecord { name: "Thomas", id: 2 }
  , TestRecord { name: "Forest", id: 3 }
  ]
