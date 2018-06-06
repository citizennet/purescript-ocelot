module UIGuide.Components.ExpansionCards where

import Prelude

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff.Timer (TIMER)
import DOM (DOM)
import Data.Array (head, take)
import Data.Either.Nested (Either2)
import Data.Functor.Coproduct.Nested (Coproduct3)
import Data.Lens (Lens', over)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax (AJAX)
import Network.RemoteData (RemoteData(..))
import Ocelot.Block.Card as Card
import Ocelot.Block.Expandable as Expandable
import Ocelot.Block.FormField as FormField
import Ocelot.Block.Icon as Icon
import Ocelot.Block.Toggle as Toggle
import Ocelot.Block.Format as Format
import Ocelot.Components.Typeahead.Input as TA
import Ocelot.Components.Typeahead as TACore
import UIGuide.Block.Backdrop as Backdrop
import UIGuide.Block.Documentation as Documentation
import UIGuide.Utilities.Async as Async


----------
-- Component Types

type State =
  { singleLocation :: Expandable.Status
  , singleUser :: Expandable.Status
  , multiLocation :: Expandable.Status
  , multiUser :: Expandable.Status
  }

data Query a
  = NoOp a
  | HandleTypeaheadUser Int (TACore.Message Query Async.User) a
  | HandleTypeaheadLocation Int (TACore.Message Query Async.Location) a
  | ToggleCard (Lens' State Expandable.Status) a
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
  { initialState
  , render
  , eval
  , receiver: const Nothing
  , initializer: Just $ H.action Initialize
  , finalizer: Nothing
  }
  where
    initialState _ =
      { singleLocation: Expandable.Collapsed
      , singleUser: Expandable.Expanded
      , multiLocation: Expandable.Collapsed
      , multiUser: Expandable.Expanded
      }
    -- For the sake of testing and visual demonstration, we'll just render
    -- out a bunch of selection variants in respective slots
    render
      :: State
      -> H.ParentHTML Query (ChildQuery (Effects eff) m) ChildSlot m
    render = cnDocumentationBlocks

    eval
      :: Query
      ~> H.ParentDSL State Query (ChildQuery (Effects eff) m) ChildSlot Void m
    eval (NoOp next) = pure next


    -- No longer necessary to fetch data. Treat it just like a Sync typeahead.
    eval (HandleTypeaheadUser slot m next) = pure next
    eval (HandleTypeaheadLocation slot m next) = pure next

    eval (ToggleCard lens next) = do
      st <- H.get
      H.put (over lens not st)
      pure next

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

_singleLocation :: Lens' State Expandable.Status
_singleLocation = prop (SProxy :: SProxy "singleLocation")

_singleUser :: Lens' State Expandable.Status
_singleUser = prop (SProxy :: SProxy "singleUser")

_multiLocation :: Lens' State Expandable.Status
_multiLocation = prop (SProxy :: SProxy "multiLocation")

_multiUser :: Lens' State Expandable.Status
_multiUser = prop (SProxy :: SProxy "multiUser")

cnDocumentationBlocks :: ∀ eff m
  . MonadAff (Effects eff) m
 => State
 -> H.ParentHTML Query (ChildQuery (Effects eff) m) ChildSlot m
cnDocumentationBlocks st =
  let css :: ∀ p i. String -> H.IProp ( "class" :: String | p ) i
      css = HP.class_ <<< HH.ClassName
    in
  HH.div_
    [ Documentation.customBlock_
      { header: "Expansion Cards"
      , subheader: "Hide sections of UI behind a click, or allow them to be hidden with one."
      }
      [ Documentation.callout_
        [ Backdrop.backdropWhite
          [ css "flex-col" ]
          [ Format.subHeading_
            [ Icon.info [ css "text-yellow pr-2" ]
            , HH.text "Animation Warning"
            ]
          , Format.p_
            [ HH.text "If the expansion card or any of its parents have "
            , HH.code_ [ HH.text "overflow: hidden" ]
            , HH.text " set, it may cause the collapse and expand animations to fail in some browsers, resulting in a rougher transition."
            ]
          ]
        ]
      , Documentation.callout_
        [ Backdrop.backdrop_
          [ Backdrop.content_
            [ Card.card_
              [ Expandable.heading
                [ HE.onClick
                  $ HE.input_
                  $ ToggleCard _singleLocation
                , Expandable.status st.singleLocation
                ]
                [ Format.subHeading_ [ HH.text "Locations" ]
                , Format.p_ [ HH.text "Here are some location typeaheads for you. Initially hidden from view since you may not be interested in them." ]
                ]
              , Expandable.content_
                st.singleLocation
                [ FormField.field_
                  { label: "Primary Location"
                  , helpText: Just "Search your favorite destination."
                  , error: Nothing
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
                , FormField.field_
                  { label: "Secondary Location"
                  , helpText: Just "Search your favorite destination."
                  , error: Nothing
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
            ]
          , Backdrop.content_
            [ Card.card_
              [ Expandable.heading
                [ HE.onClick
                  $ HE.input_
                  $ ToggleCard _singleUser
                , Expandable.status st.singleUser
                ]
                [ Format.subHeading_ [ HH.text "Users" ] ]
              , Expandable.content_
                st.singleUser
                [ FormField.field_
                  { label: "Primary User"
                  , helpText: Just "Search your favorite companion."
                  , error: Nothing
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
                , FormField.field_
                  { label: "Secondary User"
                  , helpText: Just "Search your favorite companion."
                  , error: Nothing
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
        ]
      ]
    , Documentation.block_
      { header: "Expansion Cards - Custom"
      , subheader: "Take control of how the expansion toggle looks and behaves."
      }
      [ Backdrop.backdrop_
        [ Backdrop.content_
          [ Card.card_
            [ Format.subHeading_
              [ HH.text "Optimization Rules Engine" ]
            , Format.p_
              [ HH.text "Unlock even more optimizations with customizable controls and preferences. You'll be able to tailor optimizations with greater precision towards achieving your goal. Best suited for campaigns with flexible budgets per campaign, instead use the budget optimization setting located on the Spend Tab off the Campaign Form." ]
            , FormField.field_
              { label: "Enabled"
              , helpText: Nothing
              , error: Nothing
              , inputId: "enable-locations"
              }
              [ Toggle.toggle
                [ HP.id_ "enable-locations"
                , HP.checked
                  $ Expandable.toBoolean st.multiLocation
                , HE.onChange
                  $ HE.input_
                  $ ToggleCard _multiLocation
                ]
              ]
            , Expandable.content_
              st.multiLocation
              [ FormField.field_
                { label: "Targeted Locations"
                , helpText: Just "Search your top destinations."
                , error: Nothing
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
              , FormField.field_
                { label: "Excluded Locations"
                , helpText: Just "Search your top destinations."
                , error: Nothing
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
          ]
        , Backdrop.content_
          [ Card.card_
            [ Format.subHeading_
              [ HH.text "Optimization Rules Engine" ]
            , Format.p_
              [ HH.text "Unlock even more optimizations with customizable controls and preferences. You'll be able to tailor optimizations with greater precision towards achieving your goal. Best suited for campaigns with flexible budgets per campaign, instead use the budget optimization setting located on the Spend Tab off the Campaign Form." ]
            , FormField.field_
              { label: "Enabled"
              , helpText: Nothing
              , error: Nothing
              , inputId: "enable-users"
              }
              [ Toggle.toggle
                [ HP.id_ "enable-users"
                , HP.checked
                  $ Expandable.toBoolean st.multiUser
                , HE.onChange
                  $ HE.input_
                  $ ToggleCard _multiUser
                ]
              ]
            , Expandable.content_
              st.multiUser
              [ FormField.field_
                { label: "Targeted Users"
                , helpText: Just "Search your top companions."
                , error: Nothing
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
              , FormField.field_
                { label: "Excluded Users"
                , helpText: Just "Search your top companions."
                , error: Nothing
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
    ]
