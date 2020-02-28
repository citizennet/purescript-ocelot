module UIGuide.Component.Typeaheads where

import Prelude

import Control.Parallel as Control.Parallel
import Data.Array (head, take)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..))
import Ocelot.Block.Card as Card
import Ocelot.Block.FormField as FormField
import Ocelot.Block.Format as Format
import Ocelot.Block.ItemContainer (boldMatches) as IC
import Ocelot.Component.Typeahead (Insertable(..))
import Ocelot.Component.Typeahead as TA
import Ocelot.Component.Typeahead.Render as TARender
import Ocelot.HTML.Properties (css)
import UIGuide.Block.Backdrop as Backdrop
import UIGuide.Block.Documentation as Documentation
import UIGuide.Utility.Async as Async

----------
-- Component Types

type State = Unit

data Query a
data Action
  = Initialize

----------
-- Child paths

type ChildSlot =
  ( cp1 :: TA.Slot Action Maybe Async.User Int
  , cp2 :: TA.Slot Action Array Async.User Int
  , cp3 :: TA.Slot Action Maybe Async.Location Int
  , cp4 :: TA.Slot Action Array Async.Location Int
  )

_singleUser = SProxy :: SProxy "cp1"
_multiUser = SProxy :: SProxy "cp2"
_singleLocation = SProxy :: SProxy "cp3"
_multiLocation = SProxy :: SProxy "cp4"


----------
-- Component definition

component :: ∀ m
  . MonadAff m
 => H.Component HH.HTML Query Unit Void m
component =
  H.mkComponent
  { initialState: const unit
  , render
  , eval: H.mkEval $ H.defaultEval
    { initialize = Just Initialize
    , handleAction = handleAction
    }
  }
  where
    -- For the sake of testing and visual demonstration, we'll just render
    -- out a bunch of selection variants in respective slots
    render
      :: State
      -> H.ComponentHTML Action ChildSlot m
    render _ = cnDocumentationBlocks

    handleAction
      :: Action
      -> H.HalogenM State Action ChildSlot Void m Unit
    handleAction = case _ of
      Initialize -> do
        _ <- Control.Parallel.parSequence_
          [ H.queryAll _singleUser $ H.tell $ TA.ReplaceItems Loading
          , H.queryAll _multiUser $ H.tell $ TA.ReplaceItems Loading
          , H.queryAll _singleLocation $ H.tell $ TA.ReplaceItems Loading
          , H.queryAll _multiLocation $ H.tell $ TA.ReplaceItems Loading
          ]

        _ <- Control.Parallel.parSequence_
          [ fetchAndSetLocations, fetchAndSetUsers ]

        _ <- Control.Parallel.parSequence_
          [ H.query _singleLocation 4 $ H.tell $ TA.ReplaceItems $ Failure ""
          , H.query _singleLocation 5 $ H.tell $ TA.ReplaceItems Loading
          , H.query _multiUser 4 $ H.tell $ TA.ReplaceItems $ Failure ""
          , H.query _multiUser 5 $ H.tell $ TA.ReplaceItems Loading
          ]

        pure unit

    fetchAndSetLocations = do
      remoteLocations <- H.liftAff $ Async.loadFromSource Async.locations ""
      selectedLocations <- H.liftAff $ Async.loadFromSource Async.locations "an"

      case remoteLocations, selectedLocations of
        items@(Success _), Success xs -> do
          void $ H.queryAll _singleLocation $ H.tell $ TA.ReplaceItems items
          void $ H.query _singleLocation 1 $ TA.ReplaceSelected (head xs) unit
          void $ H.query _singleLocation 3 $ TA.ReplaceSelected (head xs) unit
          void $ H.queryAll _multiLocation $ H.tell $ TA.ReplaceItems items
          void $ H.query _multiLocation 1 $ TA.ReplaceSelected (take 4 xs) unit
        _, _ -> pure unit

    fetchAndSetUsers = do
      remoteUsers <- H.liftAff $ Async.loadFromSource Async.users ""
      selectedUsers <- H.liftAff $ Async.loadFromSource Async.users "an"

      case remoteUsers, selectedUsers of
        items@(Success _), Success xs -> do
          void $ H.queryAll _singleUser $ H.tell $ TA.ReplaceItems items
          void $ H.queryAll _multiUser $ H.tell $ TA.ReplaceItems items
          void $ H.query _singleUser 1 $ TA.ReplaceSelected (head xs) unit
          void $ H.query _multiUser 1 $ TA.ReplaceSelected (take 4 xs) unit
          void $ H.query _multiUser 3 $ TA.ReplaceSelected (take 4 xs) unit
        _, _ -> pure unit

----------
-- HTML

content :: ∀ p i. Array (HH.HTML p i) -> HH.HTML p i
content = Backdrop.content [ css "flex" ]

cnDocumentationBlocks :: ∀ m
  . MonadAff m
 => H.ComponentHTML Action ChildSlot m
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
              [ HP.classes Format.captionClasses ]
              [ HH.text "Standard" ]
            , FormField.field_
              { label: HH.text "Locations"
              , helpText: [ HH.text "Search your favorite destination." ]
              , error: []
              , inputId: "location"
              }
              [ HH.slot _singleLocation 0 TA.single
                ( ( TA.syncSingle
                    { renderFuzzy: HH.span_ <<< IC.boldMatches "name"
                    , itemToObject: Async.locationToObject
                    }
                    [ HP.placeholder "Search locations..."
                    , HP.id_ "location"
                    ]
                  ) { insertable = Insertable Async.stringToLocation }
                )
                ( const Nothing )
              ]
            , HH.h3
              [ HP.classes Format.captionClasses ]
              [ HH.text "Standard Hydrated" ]
            , FormField.field_
              { label: HH.text "Locations"
              , helpText: [ HH.text "Search your favorite destination." ]
              , error: []
              , inputId: "location-hydrated"
              }
              [ HH.slot _singleLocation 1 TA.single
                ( ( TA.asyncSingle
                    { renderFuzzy: HH.span_ <<< IC.boldMatches "name"
                    , itemToObject: Async.locationToObject
                    , async: Async.loadFromSource Async.locations
                    }
                    [ HP.placeholder "Search locations..."
                    , HP.id_ "location-hydrated"
                    ]
                  ) { insertable = Insertable Async.stringToLocation }
                )
                ( const Nothing )
              ]
            ]
          ]
        , content
          [ Card.card
            [ HP.class_ $ HH.ClassName "flex-1" ]
            [ HH.h3
              [ HP.classes Format.captionClasses ]
              [ HH.text "Custom Render" ]
            , FormField.field_
              { label: HH.text "Users"
              , helpText: [ HH.text "Search your favorite companion." ]
              , error: []
              , inputId: "user"
              }
              [ HH.slot _singleUser 0 TA.single
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
            , HH.h3
              [ HP.classes Format.captionClasses ]
              [ HH.text "Custom Render Hydrated" ]
            , FormField.field_
              { label: HH.text "Users"
              , helpText: [ HH.text "Search your favorite companion." ]
              , error: []
              , inputId: "user-hydrated"
              }
              [ HH.slot _singleUser 1 TA.single
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
    , Documentation.block_
      { header: "Typeaheads - Multi-Select"
      , subheader: "Uses string input to search predetermined entries. User selects one or more of these entries"
      }
      [ Backdrop.backdrop_
        [ content
          [ Card.card
            [ HP.class_ $ HH.ClassName "flex-1" ]
            [ HH.h3
              [ HP.classes Format.captionClasses ]
              [ HH.text "Standard" ]
            , FormField.field_
              { label: HH.text "Locations"
              , helpText: [ HH.text "Search your top destinations." ]
              , error: []
              , inputId: "locations"
              }
              [ HH.slot _multiLocation 0 TA.multi
                ( ( TA.asyncMulti
                    { renderFuzzy: HH.span_ <<< IC.boldMatches "name"
                    , itemToObject: Async.locationToObject
                    , async: Async.loadFromSource Async.locations
                    }
                    [ HP.placeholder "Search locations..."
                    , HP.id_ "locations"
                    ]
                  ) { insertable = Insertable Async.stringToLocation }
                )
                ( const Nothing )
              ]
            , HH.h3
              [ HP.classes Format.captionClasses ]
              [ HH.text "Standard Hydrated" ]
            , FormField.field_
              { label: HH.text "Locations"
              , helpText: [ HH.text "Search your top destinations." ]
              , error: []
              , inputId: "locations"
              }
              [ HH.slot _multiLocation 1 TA.multi
                ( ( TA.asyncMulti
                    { renderFuzzy: HH.span_ <<< IC.boldMatches "name"
                    , itemToObject: Async.locationToObject
                    , async: Async.loadFromSource Async.locations
                    }
                    [ HP.placeholder "Search locations..."
                    , HP.id_ "locations"
                    ]
                  ) { insertable = Insertable Async.stringToLocation }
                )
                ( const Nothing )
              ]
            ]
          ]
        , content
          [ Card.card
            [ HP.class_ $ HH.ClassName "flex-1" ]
            [ HH.h3
              [ HP.classes Format.captionClasses ]
              [ HH.text "Custom Render" ]
            , FormField.field_
              { label: HH.text "Users"
              , helpText: [ HH.text "Search your top companions." ]
              , error: []
              , inputId: "users"
              }
              [ HH.slot _multiUser 0 TA.multi
                ( TA.asyncMulti
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
            , HH.h3
              [ HP.classes Format.captionClasses ]
              [ HH.text "Custom Render Hydrated" ]
            , FormField.field_
              { label: HH.text "Users"
              , helpText: [ HH.text "Search your top companions." ]
              , error: []
              , inputId: "users-hydrated"
              }
              [ HH.slot _multiUser 1 TA.multi
                ( TA.asyncMulti
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
            ]
          ]
        ]
      ]
    , Documentation.block_
      { header: "Typeaheads - State Variants"
      , subheader: "Typeaheads can also be in a disabled, loading or error state."
      }
      [ Backdrop.backdrop_
        [ content
          [ Card.card
            [ HP.class_ $ HH.ClassName "flex-1" ]
            [ HH.h3
              [ HP.classes Format.captionClasses ]
              [ HH.text "Disabled Single Select - Empty" ]
            , FormField.field_
              { label: HH.text "Locations"
              , helpText: [ HH.text "Search your top destinations." ]
              , error: []
              , inputId: "disabled-locations-empty"
              }
              [ HH.slot _singleLocation 2 TA.single
                ( TA.asyncSingle
                  { renderFuzzy: HH.span_ <<< IC.boldMatches "name"
                  , itemToObject: Async.locationToObject
                  , async: Async.loadFromSource Async.locations
                  }
                  [ HP.placeholder "Search locations..."
                  , HP.id_ "disabled-locations-empty"
                  , HP.disabled true
                  ]
                )
                ( const Nothing )
              ]
            , HH.h3
              [ HP.classes Format.captionClasses ]
              [ HH.text "Disabled Single Select - Hydrated" ]
            , FormField.field_
              { label: HH.text "Locations"
              , helpText: [ HH.text "Search your top destinations." ]
              , error: []
              , inputId: "disabled-locations-hydrated"
              }
              [ HH.slot _singleLocation 3 TA.single
                ( TA.asyncSingle
                  { renderFuzzy: HH.span_ <<< IC.boldMatches "name"
                  , itemToObject: Async.locationToObject
                  , async: Async.loadFromSource Async.locations
                  }
                  [ HP.placeholder "Search locations..."
                  , HP.id_ "disabled-locations-hydrated"
                  , HP.disabled true
                  ]
                )
                ( const Nothing )
              ]
            , HH.h3
              [ HP.classes Format.captionClasses ]
              [ HH.text "Error Single Select" ]
            , FormField.field_
              { label: HH.text "Locations"
              , helpText: [ HH.text "Search your top destinations." ]
              , error: []
              , inputId: "error-locations"
              }
              [ HH.slot _singleLocation 4 TA.single
                ( TA.asyncSingle
                  { renderFuzzy: HH.span_ <<< IC.boldMatches "name"
                  , itemToObject: Async.locationToObject
                  , async: Async.loadFromSource Async.locations
                  }
                  [ HP.placeholder "Search locations..."
                  , HP.id_ "error-locations"
                  ]
                )
                ( const Nothing )
              ]
            , HH.h3
              [ HP.classes Format.captionClasses ]
              [ HH.text "Loading Single Select" ]
            , FormField.field_
              { label: HH.text "Locations"
              , helpText: [ HH.text "Search your top destinations." ]
              , error: []
              , inputId: "loading-locations"
              }
              [ HH.slot _singleLocation 5 TA.single
                ( TA.asyncSingle
                  { renderFuzzy: HH.span_ <<< IC.boldMatches "name"
                  , itemToObject: Async.locationToObject
                  , async: Async.loadFromSource Async.locations
                  }
                  [ HP.placeholder "Search locations..."
                  , HP.id_ "loading-locations"
                  ]
                )
                ( const Nothing )
              ]
            ]
          ]
        , content
          [ Card.card
            [ HP.class_ $ HH.ClassName "flex-1" ]
            [ HH.h3
              [ HP.classes Format.captionClasses ]
              [ HH.text "Disabled Multi Select - Empty" ]
            , FormField.field_
              { label: HH.text "Users"
              , helpText: [ HH.text "Search your top companions." ]
              , error: []
              , inputId: "disabled-users-empty"
              }
              [ HH.slot _multiUser 2 TA.multi
                ( TA.asyncMulti
                  { renderFuzzy: Async.renderFuzzyUser
                  , itemToObject: Async.userToObject
                  , async: Async.loadFromSource Async.users
                  }
                  [ HP.placeholder "Search users..."
                  , HP.disabled true
                  , HP.id_ "disabled-users-empty"
                  ]
                )
                ( const Nothing )
              ]
            , HH.h3
              [ HP.classes Format.captionClasses ]
              [ HH.text "Disabled Multi Select - Hydrated" ]
            , FormField.field_
              { label: HH.text "Users"
              , helpText: [ HH.text "Search your top companions." ]
              , error: []
              , inputId: "disabled-users-hydrated"
              }
              [ HH.slot _multiUser 3 TA.multi
                ( TA.asyncMulti
                  { renderFuzzy: Async.renderFuzzyUser
                  , itemToObject: Async.userToObject
                  , async: Async.loadFromSource Async.users
                  }
                  [ HP.placeholder "Search users..."
                  , HP.disabled true
                  , HP.id_ "disabled-users-hydrated"
                  ]
                )
                ( const Nothing )
              ]
            , HH.h3
              [ HP.classes Format.captionClasses ]
              [ HH.text "Error Multi Select" ]
            , FormField.field_
              { label: HH.text "Users"
              , helpText: [ HH.text "Search your top companions." ]
              , error: []
              , inputId: "error-users"
              }
              [ HH.slot _multiUser 4 TA.multi
                ( TA.asyncMulti
                  { renderFuzzy: Async.renderFuzzyUser
                  , itemToObject: Async.userToObject
                  , async: Async.loadFromSource Async.users
                  }
                  [ HP.placeholder "Search users..."
                  , HP.id_ "error-users"
                  ]
                )
                ( const Nothing )
              ]
            , HH.h3
              [ HP.classes Format.captionClasses ]
              [ HH.text "Loading Multi Select" ]
            , FormField.field_
              { label: HH.text "Users"
              , helpText: [ HH.text "Search your top companions." ]
              , error: []
              , inputId: "loading-users"
              }
              [ HH.slot _multiUser 5 TA.multi
                ( TA.asyncMulti
                  { renderFuzzy: Async.renderFuzzyUser
                  , itemToObject: Async.userToObject
                  , async: Async.loadFromSource Async.users
                  }
                  [ HP.placeholder "Search users..."
                  , HP.id_ "loading-users"
                  ]
                )
                ( const Nothing )
              ]
            ]
          ]
        ]
      ]


    , Documentation.block_
      { header: "Typeaheads - Alternate Renderings"
      , subheader: "Renderless allows the typeahead to take on a variety of appearances"
      }
      [ Backdrop.backdrop_
        [ content
          [ HH.div
            [ HP.class_ $ HH.ClassName "flex-1 bg-black mb-6 rounded clearfix" ]
            [ HH.div
              [ HP.class_ $ HH.ClassName "m-6" ]
              [ HH.h3
                [ HP.classes Format.captionClasses ]
                [ HH.text "Searchable Dropdown in a Header (e.g. for filtering)" ]
              , HH.slot _singleLocation 9
                ( TA.component
                  { runSelect: const <<< Just
                  , runRemove: const (const Nothing)
                  , runFilter: const
                  }
                )
                { items: NotAsked
                , insertable: NotInsertable
                , keepOpen: false
                , debounceTime: Nothing
                , async: Nothing
                , itemToObject: Async.locationToObject
                , render: TARender.renderHeaderSearchDropdown
                  "All Locations"
                  "All Locations"
                  (HH.text <<< _.name <<< unwrap)
                  (HH.span_ <<< IC.boldMatches "name")
                }
                ( const Nothing )
              ]
            ]
          ]
        , content
          [ Card.card
            [ HP.class_ $ HH.ClassName "flex-1" ]
            [ HH.h3
              [ HP.classes Format.captionClasses ]
              [ HH.text "Searchable Dropdown in a Toolbar (e.g. for filtering)" ]
            , HH.slot _singleLocation 10
              ( TA.component
                { runSelect: const <<< Just
                , runRemove: const (const Nothing)
                , runFilter: const
                }
              )
              { items: NotAsked
              , insertable: NotInsertable
              , keepOpen: false
              , debounceTime: Nothing
              , async: Nothing
              , itemToObject: Async.locationToObject
              , render: TARender.renderToolbarSearchDropdown
                "All Locations"
                "All Locations"
                (HH.text <<< _.name <<< unwrap)
                (HH.span_ <<< IC.boldMatches "name")
              }
              ( const Nothing )
            ]
          ]
        ]
      ]
    ]
