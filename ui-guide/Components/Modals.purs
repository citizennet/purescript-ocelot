module UIGuide.Components.Modals where

import Prelude

import Effect.Aff.Class (class MonadAff)
import Data.Either.Nested (Either2)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ocelot.Block.Button as Button
import Ocelot.Block.Card as Card
import Ocelot.Block.FormField as FormField
import Ocelot.Block.Modal as Modal
import Ocelot.Block.Format as Format
import Ocelot.Components.Typeahead.Input as TA
import Ocelot.Components.Typeahead as TACore
import UIGuide.Utilities.Async as Async

type State = Unit

data Query a = NoOp a

type Input = Unit

type Message = Void

type ChildSlot = Either2 Unit Unit
type ChildQuery m =
  Coproduct2
    (TACore.Query Query Async.Location Async.Err m)
    (TACore.Query Query Async.User Async.Err m)

component :: ∀ m
  . MonadAff m
 => H.Component HH.HTML Query Input Message m
component =
  H.parentComponent
    { initialState: const unit
    , render
    , eval
    , receiver: const Nothing
    }
  where
    eval :: Query ~> H.ParentDSL State Query (ChildQuery m) ChildSlot Message m
    eval = case _ of
      NoOp a -> pure a

    render :: State -> H.ParentHTML Query (ChildQuery m) ChildSlot m
    render _ =
      Modal.modal_
        [ Modal.header
          { buttons:
              [ HH.a
                [ HP.classes ( Format.linkDarkClasses <> [ HH.ClassName "mr-4" ] ) ]
                [ HH.text "Cancel" ]
              , Button.buttonPrimary_ [ HH.text "Submit" ]
              ]
          , title: [ HH.text "Editing" ]
          }
        , Modal.body_
          [ Card.card
            [ HP.class_ $ HH.ClassName "flex-1 m-10" ]
            [ HH.h3
              [ HP.classes Format.captionClasses ]
              [ HH.text "Standard" ]
            , FormField.field_
              { label: "Locations"
              , helpText: Just "Search your top destinations."
              , error: Nothing
              , inputId: "locations"
              }
              [ HH.slot' CP.cp1 unit TACore.component
                (TA.defAsyncMulti
                  [ HP.placeholder "Search locations..."
                  , HP.id_ "locations"
                  ]
                  ( Async.loadFromSource Async.locations )
                  Async.renderItemLocation
                )
                ( const Nothing )
              ]
            , HH.h3
              [ HP.classes Format.captionClasses ]
              [ HH.text "Standard Hydrated" ]
            , FormField.field_
              { label: "Locations"
              , helpText: Just "Search your top destinations."
              , error: Nothing
              , inputId: "locations"
              }
              [ HH.slot' CP.cp2 unit TACore.component
                (TA.defAsyncMulti
                  [ HP.placeholder "Search users..."
                  , HP.id_ "users"
                  ]
                  ( Async.loadFromSource Async.users )
                  Async.renderItemUser
                )
                ( const Nothing )
              ]
            ]
          ]
      ]
