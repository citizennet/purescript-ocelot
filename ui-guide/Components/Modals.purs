module UIGuide.Component.Modals where

import Prelude

import Data.Either.Nested (Either2)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Ocelot.Block.Button as Button
import Ocelot.Block.Card as Card
import Ocelot.Block.FormField as FormField
import Ocelot.Block.Format as Format
import Ocelot.Component.Typeahead as TACore
import Ocelot.Component.Typeahead.Input as TA
import Ocelot.HTML.Properties (css)
import Ocelot.Part.Modal as Modal
import UIGuide.Block.Backdrop as Backdrop
import UIGuide.Block.Documentation as Documentation
import UIGuide.Utility.Async as Async
import Web.UIEvent.KeyboardEvent as KE

type State = Boolean

data Query a
  = Open a
  | Close a
  | HandleKey KE.KeyboardEvent (H.SubscribeStatus -> a)

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
    { initialState: const false
    , render
    , eval
    , receiver: const Nothing
    }
  where
    eval :: Query ~> H.ParentDSL State Query (ChildQuery m) ChildSlot Message m
    eval = case _ of
      HandleKey ev reply -> do
        Modal.whenClose ev reply (H.put false)

      Open a -> do
        Modal.initializeWith HandleKey
        H.put true $> a

      Close a -> do
        H.put false
        pure a

    render :: State -> H.ParentHTML Query (ChildQuery m) ChildSlot m
    render isOpen =
      HH.div_
        [ Documentation.block_
          { header: "Modals"
          , subheader: "Forest's favorite UI implement"
          }
          [ Backdrop.backdrop_
            [ Backdrop.content
              [ css "mt-0 text-center" ]
              [ Button.button
                [ HE.onClick $ HE.input_ Open ]
                [ HH.text "Open Modal" ]
              ]
            ]
          ]
        , if isOpen then renderModal else HH.text ""
        ]

    renderModal =
      Modal.modal_ Close
        [ Modal.header
          { buttons:
              [ HH.a
                [ HP.classes ( Format.linkDarkClasses <> [ HH.ClassName "mr-4" ] )
                , HE.onClick $ HE.input_ Close ]
                [ HH.text "Cancel" ]
              , Button.buttonPrimary
                [ HE.onClick $ HE.input_ Close ]
                [ HH.text "Submit" ]
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
              { label: HH.text "Locations"
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
              { label: HH.text "Locations"
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
