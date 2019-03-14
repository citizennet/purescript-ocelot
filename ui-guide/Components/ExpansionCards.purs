module UIGuide.Component.ExpansionCards where

import Prelude

import Data.Array (head, take)
import Data.Either.Nested (Either5)
import Data.Functor.Coproduct.Nested (Coproduct5)
import Data.Lens (Lens', over)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..))
import Ocelot.Block.Card as Card
import Ocelot.Block.Expandable as Expandable
import Ocelot.Block.FormField as FormField
import Ocelot.Block.Format as Format
import Ocelot.Block.Icon as Icon
import Ocelot.Block.ItemContainer (boldMatches) as IC
import Ocelot.Block.Toggle as Toggle
import Ocelot.Component.Typeahead as TA
import Ocelot.HTML.Properties (css)
import UIGuide.Block.Backdrop as Backdrop
import UIGuide.Block.Documentation as Documentation
import UIGuide.Utility.Async as Async


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
  | ToggleCard (Lens' State Expandable.Status) a
  | Initialize a

----------
-- Child paths

type ChildSlot = Either5 Int Int Int Int Unit
type ChildQuery m =
  Coproduct5
    (TA.Query Query Maybe Async.User m)
    (TA.Query Query Array Async.User m)
    (TA.Query Query Maybe Async.Location m)
    (TA.Query Query Array Async.Location m)
    Query


----------
-- Component definition

component :: ∀ m
  . MonadAff m
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
      -> H.ParentHTML Query (ChildQuery m) ChildSlot m
    render = cnDocumentationBlocks

    eval
      :: Query
      ~> H.ParentDSL State Query (ChildQuery m) ChildSlot Void m
    eval (NoOp next) = pure next

    eval (ToggleCard lens next) = do
      st <- H.get
      H.put (over lens not st)
      pure next

    eval (Initialize next) = do
      _ <- H.queryAll' CP.cp1 $ H.action $ TA.ReplaceItems Loading
      _ <- H.queryAll' CP.cp2 $ H.action $ TA.ReplaceItems Loading
      _ <- H.queryAll' CP.cp3 $ H.action $ TA.ReplaceItems Loading
      _ <- H.queryAll' CP.cp4 $ H.action $ TA.ReplaceItems Loading

      remoteLocations <- H.liftAff $ Async.loadFromSource Async.locations ""
      _ <- case remoteLocations of
        items@(Success _) -> do
          _ <- H.queryAll' CP.cp3 $ H.action $ TA.ReplaceItems items
          _ <- H.queryAll' CP.cp4 $ H.action $ TA.ReplaceItems items
          pure unit
        otherwise -> pure unit

      remoteUsers <- H.liftAff $ Async.loadFromSource Async.users ""
      _ <- case remoteUsers of
        items@(Success _) -> do
          _ <- H.queryAll' CP.cp1 $ H.action $ TA.ReplaceItems items
          _ <- H.queryAll' CP.cp2 $ H.action $ TA.ReplaceItems items
          pure unit
        otherwise -> pure unit

      selectedLocations <- H.liftAff $ Async.loadFromSource Async.locations "an"
      _ <- case selectedLocations of
        Success xs -> do
          _ <- H.query' CP.cp3 1 $ H.action $ TA.ReplaceSelected (head xs)
          _ <- H.query' CP.cp4 3 $ H.action $ TA.ReplaceSelected (take 4 xs)
          pure unit
        otherwise -> pure unit

      selectedUsers <- H.liftAff $ Async.loadFromSource Async.users "an"
      case selectedUsers of
        Success xs -> do
          _ <- H.query' CP.cp1 1 $ H.action $ TA.ReplaceSelected (head xs)
          _ <- H.query' CP.cp2 3 $ H.action $ TA.ReplaceSelected (take 4 xs)
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

cnDocumentationBlocks :: ∀ m
  . MonadAff m
 => State
 -> H.ParentHTML Query (ChildQuery m) ChildSlot m
cnDocumentationBlocks st =
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
                  { label: HH.text "Primary Location"
                  , helpText: [ HH.text "Search your favorite destination." ]
                  , error: []
                  , inputId: "location"
                  }
                  [ HH.slot' CP.cp3 0 TA.single
                    ( TA.asyncSingle
                      { renderFuzzy: HH.span_ <<< IC.boldMatches "name"
                      , itemToObject: Async.locationToObject
                      , async: Async.loadFromSource Async.locations
                      }
                      [ HP.placeholder "Search locations..."
                      , HP.id_ "location"
                      ]
                    )
                    ( const Nothing )
                  ]
                , FormField.field_
                  { label: HH.text "Secondary Location"
                  , helpText: [ HH.text "Search your favorite destination." ]
                  , error: []
                  , inputId: "location-hydrated"
                  }
                  [ HH.slot' CP.cp3 1 TA.single
                    ( TA.asyncSingle
                      { renderFuzzy: HH.span_ <<< IC.boldMatches "name"
                      , itemToObject: Async.locationToObject
                      , async: Async.loadFromSource Async.locations
                      }
                      [ HP.placeholder "Search locations..."
                      , HP.id_ "location-hydrated"
                      ]
                    )
                    ( const Nothing )
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
                  { label: HH.text "Primary User"
                  , helpText: [ HH.text "Search your favorite companion." ]
                  , error: []
                  , inputId: "user"
                  }
                  [ HH.slot' CP.cp1 0 TA.single
                    ( TA.asyncSingle
                      { renderFuzzy: Async.renderFuzzyUser
                      , itemToObject: Async.userToObject
                      , async: Async.loadFromSource Async.users
                      }
                      [ HP.placeholder "Search users..."
                      , HP.id_ "user"
                      ]
                    )
                    ( const Nothing )
                  ]
                , FormField.field_
                  { label: HH.text "Secondary User"
                  , helpText: [ HH.text "Search your favorite companion." ]
                  , error: []
                  , inputId: "user-hydrated"
                  }
                  [ HH.slot' CP.cp1 1 TA.single
                    ( TA.asyncSingle
                      { renderFuzzy: Async.renderFuzzyUser
                      , itemToObject: Async.userToObject
                      , async: Async.loadFromSource Async.users
                      }
                      [ HP.placeholder "Search users..."
                      , HP.id_ "user-hydrated"
                      ]
                    )
                    ( const Nothing )
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
              { label: HH.text "Enabled"
              , helpText: []
              , error: []
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
                { label: HH.text "Targeted Locations"
                , helpText: [ HH.text "Search your top destinations." ]
                , error: []
                , inputId: "locations"
                }
                [ HH.slot' CP.cp4 0 TA.multi
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
              , FormField.field_
                { label: HH.text "Excluded Locations"
                , helpText: [ HH.text "Search your top destinations." ]
                , error: []
                , inputId: "locations"
                }
                [ HH.slot' CP.cp4 1 TA.multi
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
              { label: HH.text "Enabled"
              , helpText: []
              , error: []
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
                { label: HH.text "Targeted Users"
                , helpText: [ HH.text "Search your top companions." ]
                , error: []
                , inputId: "users"
                }
                [ HH.slot' CP.cp2 0 TA.multi
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
              , FormField.field_
                { label: HH.text "Excluded Users"
                , helpText: [ HH.text "Search your top companions." ]
                , error: []
                , inputId: "users-hydrated"
                }
                [ HH.slot' CP.cp2 1 TA.multi
                  ( TA.asyncMulti
                    { renderFuzzy: Async.renderFuzzyUser
                    , itemToObject: Async.userToObject
                    , async: Async.loadFromSource Async.users
                    }
                    [ HP.placeholder "Search users..."
                    , HP.id_ "users-hydrated"
                    ]
                  )
                  ( const Nothing )
                ]
              ]
            ]
          ]
        ]
      ]
    ]
