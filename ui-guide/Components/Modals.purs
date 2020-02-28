module UIGuide.Component.Modals where

import Prelude

import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), isJust)
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Ocelot.Block.Button as Button
import Ocelot.Block.Card as Card
import Ocelot.Block.FormField as FormField
import Ocelot.Block.Format as Format
import Ocelot.Block.ItemContainer (boldMatches) as IC
import Ocelot.Component.Typeahead as TA
import Ocelot.HTML.Properties (css)
import Ocelot.Part.Modal as Modal
import UIGuide.Block.Backdrop as Backdrop
import UIGuide.Block.Documentation as Documentation
import UIGuide.Utility.Async as Async
import Web.UIEvent.KeyboardEvent as KE

type State = Maybe H.SubscriptionId

data Query a
data Action
  = Open
  | Close
  | HandleKey KE.KeyboardEvent

type Input = Unit

type Message = Void

type ChildSlot =
  ( cp1 :: TA.Slot Action Array Async.Location Unit
  , cp2 :: TA.Slot Action Array Async.User Unit
  )

_cp1 = SProxy :: SProxy "cp1"
_cp2 = SProxy :: SProxy "cp2"

component :: ∀ m
  . MonadAff m
 => H.Component HH.HTML Query Input Message m
component =
  H.mkComponent
    { initialState: const Nothing
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
    handleAction :: Action -> H.HalogenM State Action ChildSlot Message m Unit
    handleAction = case _ of
      HandleKey ev -> do
        id <- H.get
        traverse_ (\sid -> Modal.whenClose ev sid $ handleAction Close) id

      Open -> do
        id <- Modal.initializeWith (Just <<< HandleKey)
        H.put $ Just id

      Close -> do
        H.put Nothing

    render :: State -> H.ComponentHTML Action ChildSlot m
    render st =
      HH.div_
        [ Documentation.block_
          { header: "Modals"
          , subheader: "Forest's favorite UI implement"
          }
          [ Backdrop.backdrop_
            [ Backdrop.content
              [ css "mt-0 text-center" ]
              [ Button.button
                [ HE.onClick $ const $ Just Open ]
                [ HH.text "Open Modal" ]
              ]
            ]
          ]
        , if isOpen then renderModal else HH.text ""
        ]
      where
      isOpen = isJust st

    renderModal =
      Modal.modal_ Close
        [ Modal.header
          { buttons:
              [ HH.a
                [ HP.classes ( Format.linkDarkClasses <> [ HH.ClassName "mr-4" ] )
                , HE.onClick $ const $ Just Close ]
                [ HH.text "Cancel" ]
              , Button.buttonPrimary
                [ HE.onClick $ const $ Just Close ]
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
              , helpText: [ HH.text "Search your top destinations." ]
              , error: []
              , inputId: "locations"
              }
              [ HH.slot _cp1 unit TA.multi
                ( TA.asyncMulti
                  { renderFuzzy: HH.span_ <<< IC.boldMatches "name"
                  , itemToObject: Async.locationToObject
                  , async: Async.loadFromSource Async.locations
                  }
                  [ HP.placeholder "Search locations..."
                  , HP.id_ "locations"
                  ]
                )
                ( const Nothing )
              ]
            , HH.h3
              [ HP.classes Format.captionClasses ]
              [ HH.text "Standard Hydrated" ]
            , FormField.field_
              { label: HH.text "Users"
              , helpText: [ HH.text "Search your favorite companion." ]
              , error: []
              , inputId: "users"
              }
              [ HH.slot _cp2 unit TA.multi
                ( TA.asyncMulti
                  { renderFuzzy: Async.renderFuzzyUser
                  , itemToObject: Async.userToObject
                  , async: Async.loadFromSource Async.users
                  }
                  [ HP.placeholder "Search users..."
                  , HP.id_ "users"
                  ]
                )
                ( const Nothing )
              ]
            ]
          ]
        ]
