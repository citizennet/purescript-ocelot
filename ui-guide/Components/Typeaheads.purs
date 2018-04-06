module UIGuide.Components.Typeaheads where

import Prelude

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff.Timer (TIMER)
import DOM (DOM)
import Data.Array (head, take)
import Data.Either (either, note)
import Data.Either.Nested (Either4, Either8, Either2)
import Data.Functor.Coproduct.Nested (Coproduct2, Coproduct4, Coproduct8, Coproduct3)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Data.Traversable (traverse)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax (AJAX)
import Network.RemoteData (RemoteData(..))
import Ocelot.Block.Card as Card
import Ocelot.Block.FormField as FormField
import Ocelot.Block.Input as Input
import Ocelot.Block.Type as Type
import Ocelot.Components.Typeahead as TA
import Ocelot.Core.Typeahead as TACore
import Ocelot.Core.Validation as Validation
import UIGuide.Block.Backdrop as Backdrop
import UIGuide.Block.Documentation as Documentation
import UIGuide.Utilities.Async as Async


----------
-- Component Types

type State = Unit

data Query a
  = NoOp a
  | HandleTypeaheadUser Int (TACore.Message Query Async.User) a
  | HandleTypeaheadLocation Int (TACore.Message Query Async.Location) a
  | Initialize a

----------
-- Child paths

type ChildSlot = Either2 Int Int
type ChildQuery eff m =
  Coproduct3
    (TACore.Query Query Async.Location Async.Err eff m)
    (TACore.Query Query Async.User Async.Err eff m)
    Query


----------
-- Component definition

-- NOTE: Uses the same effects but does not compose with typeahead effects. Written out again from scratch.
type Effects eff =
  ( avar :: AVAR
  , dom :: DOM
  , ajax :: AJAX
  , timer :: TIMER
  , console :: CONSOLE
  | eff
  )

component :: ∀ eff m
  . MonadAff (Effects eff) m
 => H.Component HH.HTML Query Unit Void m
component =
  H.lifecycleParentComponent
  { initialState: const unit
  , render
  , eval
  , receiver: const Nothing
  , initializer: Just $ H.action Initialize
  , finalizer: Nothing
  }
  where
    -- For the sake of testing and visual demonstration, we'll just render
    -- out a bunch of selection variants in respective slots
    render
      :: State
      -> H.ParentHTML Query (ChildQuery (Effects eff) m) ChildSlot m
    render _ = cnDocumentationBlocks

    eval
      :: Query
      ~> H.ParentDSL State Query (ChildQuery (Effects eff) m) ChildSlot Void m
    eval (NoOp next) = pure next


    -- No longer necessary to fetch data. Treat it just like a Sync typeahead.
    eval (HandleTypeaheadUser slot m next) = pure next
    eval (HandleTypeaheadLocation slot m next) = pure next

    eval (Initialize next) = do
      _ <- H.queryAll' CP.cp1 $ H.action $ TACore.ReplaceItems Loading
      _ <- H.queryAll' CP.cp2 $ H.action $ TACore.ReplaceItems Loading
      remoteLocations <- H.liftAff $ Async.loadFromSource Async.locations ""
      _ <- case remoteLocations of
        items@(Success _) -> do
          _ <- H.queryAll' CP.cp1 $ H.action $ TACore.ReplaceItems items
          pure unit
        otherwise -> pure unit
      remoteUsers <- H.liftAff $ Async.loadFromSource Async.users ""
      _ <- case remoteUsers of
        items@(Success _) -> do
          _ <- H.queryAll' CP.cp2 $ H.action $ TACore.ReplaceItems items
          pure unit
        otherwise -> pure unit
      selectedLocations <- H.liftAff $ Async.loadFromSource Async.locations "an"
      _ <- case selectedLocations of
        Success xs -> do
          _ <- H.query' CP.cp1 1
            $ H.action
            $ TACore.ReplaceSelections
            $ TACore.One
            $ head xs
          _ <- H.query' CP.cp1 3
            $ H.action
            $ TACore.ReplaceSelections
            $ TACore.Many
            $ take 4 xs
          pure unit
        otherwise -> pure unit
      selectedUsers <- H.liftAff $ Async.loadFromSource Async.users "an"
      case selectedUsers of
        Success xs -> do
          _ <- H.query' CP.cp2 1
            $ H.action
            $ TACore.ReplaceSelections
            $ TACore.One
            $ head xs
          _ <- H.query' CP.cp2 3
            $ H.action
            $ TACore.ReplaceSelections
            $ TACore.Many
            $ take 4 xs
          pure next
        otherwise -> pure next

----------
-- HTML

css :: ∀ t0 t1. String -> H.IProp ( "class" :: String | t0 ) t1
css = HP.class_ <<< HH.ClassName

content :: ∀ p i. Array (HH.HTML p (i Unit)) -> HH.HTML p (i Unit)
content = Backdrop.content [ css "flex" ]

cnDocumentationBlocks :: ∀ eff m
  . MonadAff (Effects eff) m
 => H.ParentHTML Query (ChildQuery (Effects eff) m) ChildSlot m
cnDocumentationBlocks =
  HH.div_
    [ Documentation.block_
      { header: "Typeaheads - Single-Select"
      , subheader: "Uses string input to search predetermined entries. User selects one of these entries."
      }
      [ Backdrop.backdrop_
        [ content
          [ Card.card
            [ HP.class_ $ HH.ClassName "flex-1" ]
            [ HH.h3
              [ HP.classes Type.captionClasses ]
              [ HH.text "Standard" ]
            , FormField.field_
              { label: "Locations"
              , helpText: Just "Search your favorite destination."
              , valid: Nothing
              , inputId: "location"
              }
              [ HH.slot' CP.cp1 0 TACore.component
                (TA.defAsyncSingle
                  [ HP.placeholder "Search locations..."
                  , HP.id_ "location"
                  ]
                  ( Async.loadFromSource Async.locations )
                  Async.renderItemLocation
                )
                ( HE.input $ HandleTypeaheadLocation 0 )
              ]
            , HH.h3
              [ HP.classes Type.captionClasses ]
              [ HH.text "Standard Hydrated" ]
            , FormField.field_
              { label: "Locations"
              , helpText: Just "Search your favorite destination."
              , valid: Nothing
              , inputId: "location-hydrated"
              }
              [ HH.slot' CP.cp1 1 TACore.component
                (TA.defAsyncSingle
                  [ HP.placeholder "Search locations..."
                  , HP.id_ "location-hydrated"
                  ]
                  ( Async.loadFromSource Async.locations )
                  Async.renderItemLocation
                )
                ( HE.input $ HandleTypeaheadLocation 1 )
              ]
            ]
          ]
        , content
          [ Card.card
            [ HP.class_ $ HH.ClassName "flex-1" ]
            [ HH.h3
              [ HP.classes Type.captionClasses ]
              [ HH.text "Custom Render" ]
            , FormField.field_
              { label: "Users"
              , helpText: Just "Search your favorite companion."
              , valid: Nothing
              , inputId: "user"
              }
              [ HH.slot' CP.cp2 0 TACore.component
                (TA.defAsyncSingle
                  [ HP.placeholder "Search users..."
                  , HP.id_ "user"
                  ]
                  ( Async.loadFromSource Async.users )
                  Async.renderItemUser
                )
                ( HE.input $ HandleTypeaheadUser 0 )
              ]
            , HH.h3
              [ HP.classes Type.captionClasses ]
              [ HH.text "Custom Render Hydrated" ]
            , FormField.field_
              { label: "Users"
              , helpText: Just "Search your favorite companion."
              , valid: Nothing
              , inputId: "user-hydrated"
              }
              [ HH.slot' CP.cp2 1 TACore.component
                (TA.defAsyncSingle
                  [ HP.placeholder "Search users..."
                  , HP.id_ "user-hydrated"
                  ]
                  ( Async.loadFromSource Async.users )
                  Async.renderItemUser
                )
                ( HE.input $ HandleTypeaheadUser 1 )
              ]
            ]
          ]
        ]
      ]
    , Documentation.block_
      { header: "Typeaheads - Multi-Select"
      , subheader: "Uses string input to search predetermined entries. User selects one or more of these entries"
      }
      [ Backdrop.backdrop_
        [ content
          [ Card.card
            [ HP.class_ $ HH.ClassName "flex-1" ]
            [ HH.h3
              [ HP.classes Type.captionClasses ]
              [ HH.text "Standard" ]
            , FormField.field_
              { label: "Locations"
              , helpText: Just "Search your top destinations."
              , valid: Nothing
              , inputId: "locations"
              }
              [ HH.slot' CP.cp1 2 TACore.component
                (TA.defAsyncMulti
                  [ HP.placeholder "Search locations..."
                  , HP.id_ "locations"
                  ]
                  ( Async.loadFromSource Async.locations )
                  Async.renderItemLocation
                )
                ( HE.input $ HandleTypeaheadLocation 2 )
              ]
            , HH.h3
              [ HP.classes Type.captionClasses ]
              [ HH.text "Standard Hydrated" ]
            , FormField.field_
              { label: "Locations"
              , helpText: Just "Search your top destinations."
              , valid: Nothing
              , inputId: "locations"
              }
              [ HH.slot' CP.cp1 3 TACore.component
                (TA.defAsyncMulti
                  [ HP.placeholder "Search locations..."
                  , HP.id_ "locations"
                  ]
                  ( Async.loadFromSource Async.locations )
                  Async.renderItemLocation
                )
                ( HE.input $ HandleTypeaheadLocation 3 )
              ]
            ]
          ]
        , content
          [ Card.card
            [ HP.class_ $ HH.ClassName "flex-1" ]
            [ HH.h3
              [ HP.classes Type.captionClasses ]
              [ HH.text "Custom Render" ]
            , FormField.field_
              { label: "Users"
              , helpText: Just "Search your top companions."
              , valid: Nothing
              , inputId: "users"
              }
              [ HH.slot' CP.cp2 2 TACore.component
                (TA.defAsyncMulti
                  [ HP.placeholder "Search users..."
                  , HP.id_ "users"
                  ]
                  ( Async.loadFromSource Async.users )
                  Async.renderItemUser
                )
                ( HE.input $ HandleTypeaheadUser 2 )
              ]
            , HH.h3
              [ HP.classes Type.captionClasses ]
              [ HH.text "Custom Render Hydrated" ]
            , FormField.field_
              { label: "Users"
              , helpText: Just "Search your top companions."
              , valid: Nothing
              , inputId: "users-hydrated"
              }
              [ HH.slot' CP.cp2 3 TACore.component
                (TA.defAsyncMulti
                  [ HP.placeholder "Search users..."
                  , HP.id_ "users-hydrated"
                  ]
                  ( Async.loadFromSource Async.users )
                  Async.renderItemUser
                )
                ( HE.input $ HandleTypeaheadUser 3 )
              ]
            ]
          ]
        ]
      ]
    ]
