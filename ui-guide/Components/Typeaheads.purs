module UIGuide.Component.Typeaheads where

import Prelude

import Control.Parallel as Control.Parallel
import Data.Array (head, take)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console as Effect.Class.Console
import Foreign.Object as Foreign.Object
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..))
import Ocelot.Block.Card as Card
import Ocelot.Block.FormField as FormField
import Ocelot.Block.Format as Format
import Ocelot.Block.ItemContainer (boldMatches) as IC
import Ocelot.HTML.Properties (css)
import Ocelot.Typeahead as TA
import Type.Proxy (Proxy(..))
import UIGuide.Block.Backdrop as Backdrop
import UIGuide.Block.Documentation as Documentation
import UIGuide.Utility.Async as Async

----------
-- Component Types

type State = Unit

data Query (a :: Type)
data Action
  = Initialize
  | HandleMultiInput (TA.Output Action Array String)

----------
-- Child paths

type ChildSlot =
  ( cp1 :: TA.Slot Action Maybe Async.User Int
  , cp2 :: TA.Slot Action Array Async.User Int
  , cp3 :: TA.Slot Action Maybe Async.Location Int
  , cp4 :: TA.Slot Action Array Async.Location Int
  , multiInput :: TA.Slot Action Array String Int
  )

_singleUser = Proxy :: Proxy "cp1"
_multiUser = Proxy :: Proxy "cp2"
_singleLocation = Proxy :: Proxy "cp3"
_multiLocation = Proxy :: Proxy "cp4"
_multiInput = Proxy :: Proxy "multiInput"


----------
-- Component definition

component :: ∀ m
  . MonadAff m
 => H.Component Query Unit Void m
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
      HandleMultiInput output -> case output of
        TA.SelectionChanged _ xs -> do
          Effect.Class.Console.log $ "MultiInput Selection Changed: " <> show xs
        _ -> pure unit
      Initialize -> do
        _ <- Control.Parallel.parSequence_
          [ H.queryAll _singleUser $ H.mkTell $ TA.ReplaceItems Loading
          , H.queryAll _multiUser $ H.mkTell $ TA.ReplaceItems Loading
          , H.queryAll _singleLocation $ H.mkTell $ TA.ReplaceItems Loading
          , H.queryAll _multiLocation $ H.mkTell $ TA.ReplaceItems Loading
          ]

        _ <- Control.Parallel.parSequence_
          [ fetchAndSetLocations
          , fetchAndSetUsers
          , void $ H.queryAll _multiInput $ H.mkTell $ TA.ReplaceItems (pure ["new", "space", "citizennet", "conde"])
          , H.tell _multiInput 1 $ TA.ReplaceSelected ["new", "conde"]
          ]

        _ <- Control.Parallel.parSequence_
          [ H.query _singleLocation 4 $ H.mkTell $ TA.ReplaceItems $ Failure ""
          , H.query _singleLocation 5 $ H.mkTell $ TA.ReplaceItems Loading
          , H.query _multiUser 4 $ H.mkTell $ TA.ReplaceItems $ Failure ""
          , H.query _multiUser 5 $ H.mkTell $ TA.ReplaceItems Loading
          ]

        pure unit

    fetchAndSetLocations = do
      remoteLocations <- H.liftAff $ Async.loadFromSource Async.locations ""
      selectedLocations <- H.liftAff $ Async.loadFromSource Async.locations "an"

      case remoteLocations, selectedLocations of
        items@(Success _), Success xs -> do
          void $ H.queryAll _singleLocation $ H.mkTell $ TA.ReplaceItems items
          void $ H.query _singleLocation 1 $ TA.ReplaceSelected (head xs) unit
          void $ H.query _singleLocation 3 $ TA.ReplaceSelected (head xs) unit
          void $ H.queryAll _multiLocation $ H.mkTell $ TA.ReplaceItems items
          void $ H.query _multiLocation 1 $ TA.ReplaceSelected (take 4 xs) unit
        _, _ -> pure unit

    fetchAndSetUsers = do
      remoteUsers <- H.liftAff $ Async.loadFromSource Async.users ""
      selectedUsers <- H.liftAff $ Async.loadFromSource Async.users "an"

      case remoteUsers, selectedUsers of
        items@(Success _), Success xs -> do
          void $ H.queryAll _singleUser $ H.mkTell $ TA.ReplaceItems items
          void $ H.queryAll _multiUser $ H.mkTell $ TA.ReplaceItems items
          void $ H.tell _singleUser 1 $ TA.ReplaceSelected (head xs)
          void $ H.tell _multiUser 1 $ TA.ReplaceSelected (take 4 xs)
          void $ H.tell _multiUser 3 $ TA.ReplaceSelected (take 4 xs)
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
              [ HH.slot_ _singleLocation 0 TA.single
                ( ( TA.syncSingle
                    { renderFuzzy: HH.span_ <<< IC.boldMatches "name"
                    , itemToObject: Async.locationToObject
                    }
                    [ HP.placeholder "Search locations..."
                    , HP.id_ "location"
                    ]
                  ) { insertable = TA.Insertable Async.stringToLocation }
                )

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
              [ HH.slot_ _singleLocation 1 TA.single
                ( ( TA.asyncSingle
                    { renderFuzzy: HH.span_ <<< IC.boldMatches "name"
                    , itemToObject: Async.locationToObject
                    , async: Async.loadFromSource Async.locations
                    }
                    [ HP.placeholder "Search locations..."
                    , HP.id_ "location-hydrated"
                    ]
                  ) { insertable = TA.Insertable Async.stringToLocation }
                )
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
              [ HH.slot_ _singleUser 0 TA.singleHighlightOnly
                ( TA.asyncSingle
                  { renderFuzzy: Async.renderFuzzyUser
                  , itemToObject: Async.userToObject
                  , async: Async.loadFromSource Async.users
                  }
                  [ HP.placeholder "Search users..."
                  , HP.id_ "user"
                  ]
                )
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
              [ HH.slot_ _singleUser 1 TA.singleHighlightOnly
                ( TA.asyncSingle
                  { renderFuzzy: Async.renderFuzzyUser
                  , itemToObject: Async.userToObject
                  , async: Async.loadFromSource Async.users
                  }
                  [ HP.placeholder "Search users..."
                  , HP.id_ "user-hydrated"
                  ]
                )
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
              [ HH.slot_ _multiLocation 0 TA.multi
                ( ( TA.asyncMulti
                    { renderFuzzy: HH.span_ <<< IC.boldMatches "name"
                    , itemToObject: Async.locationToObject
                    , async: Async.loadFromSource Async.locations
                    }
                    [ HP.placeholder "Search locations..."
                    , HP.id_ "locations"
                    ]
                  ) { insertable = TA.Insertable Async.stringToLocation }
                )
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
              [ HH.slot_ _multiLocation 1 TA.multi
                ( ( TA.asyncMulti
                    { renderFuzzy: HH.span_ <<< IC.boldMatches "name"
                    , itemToObject: Async.locationToObject
                    , async: Async.loadFromSource Async.locations
                    }
                    [ HP.placeholder "Search locations..."
                    , HP.id_ "locations"
                    ]
                  ) { insertable = TA.Insertable Async.stringToLocation }
                )
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
              [ HH.slot_ _multiUser 0 TA.multiHighlightOnly
                ( TA.asyncMulti
                  { renderFuzzy: Async.renderFuzzyUser
                  , itemToObject: Async.userToObject
                  , async: Async.loadFromSource Async.users
                  }
                  [ HP.placeholder "Search users..."
                  , HP.id_ "user"
                  ]
                )
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
              [ HH.slot_ _multiUser 1 TA.multiHighlightOnly
                ( TA.asyncMulti
                  { renderFuzzy: Async.renderFuzzyUser
                  , itemToObject: Async.userToObject
                  , async: Async.loadFromSource Async.users
                  }
                  [ HP.placeholder "Search users..."
                  , HP.id_ "user"
                  ]
                )
              ]
            ]
          ]
        ]
      ]
    , Documentation.block_
      { header: "Typeaheads - Multi-Input"
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
              { label: HH.text "Tag"
              , helpText: []
              , error: []
              , inputId: "tags"
              }
              [ HH.slot _multiInput 0 TA.multi
                ( ( TA.syncMultiInput
                    { renderFuzzy: HH.span_ <<< IC.boldMatches "name"
                    , itemToObject: Foreign.Object.fromHomogeneous <<< { name: _ }
                    }
                    []
                    { minWidth: 50.0
                    , placeholder:
                      { primary: "At least one of these values..."
                      , secondary: "And..."
                      }
                    }
                  ) { insertable = TA.Insertable identity }
                )
                HandleMultiInput
              ]
            , HH.h3
              [ HP.classes Format.captionClasses ]
              [ HH.text "Standard Hydrated" ]
            , FormField.field_
              { label: HH.text "Tag"
              , helpText: []
              , error: []
              , inputId: "tags"
              }
              [ HH.slot _multiInput 1 TA.multi
                ( ( TA.syncMultiInput
                    { renderFuzzy: HH.span_ <<< IC.boldMatches "name"
                    , itemToObject: Foreign.Object.fromHomogeneous <<< { name: _ }
                    }
                    []
                    { minWidth: 50.0
                    , placeholder:
                      { primary: "At least one of these values..."
                      , secondary: "And..."
                      }
                    }
                  ) { insertable = TA.Insertable identity }
                )
                HandleMultiInput
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
              [ HH.slot_ _singleLocation 2 TA.single
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
              [ HH.slot_ _singleLocation 3 TA.single
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
              [ HH.slot_ _singleLocation 4 TA.single
                ( TA.asyncSingle
                  { renderFuzzy: HH.span_ <<< IC.boldMatches "name"
                  , itemToObject: Async.locationToObject
                  , async: Async.loadFromSource Async.locations
                  }
                  [ HP.placeholder "Search locations..."
                  , HP.id_ "error-locations"
                  ]
                )
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
              [ HH.slot_ _singleLocation 5 TA.single
                ( TA.asyncSingle
                  { renderFuzzy: HH.span_ <<< IC.boldMatches "name"
                  , itemToObject: Async.locationToObject
                  , async: Async.loadFromSource Async.locations
                  }
                  [ HP.placeholder "Search locations..."
                  , HP.id_ "loading-locations"
                  ]
                )
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
              [ HH.slot_ _multiUser 2 TA.multiHighlightOnly
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
              [ HH.slot_ _multiUser 3 TA.multiHighlightOnly
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
              [ HH.slot_ _multiUser 4 TA.multiHighlightOnly
                ( TA.asyncMulti
                  { renderFuzzy: Async.renderFuzzyUser
                  , itemToObject: Async.userToObject
                  , async: Async.loadFromSource Async.users
                  }
                  [ HP.placeholder "Search users..."
                  , HP.id_ "error-users"
                  ]
                )
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
              [ HH.slot_ _multiUser 5 TA.multiHighlightOnly
                ( TA.asyncMulti
                  { renderFuzzy: Async.renderFuzzyUser
                  , itemToObject: Async.userToObject
                  , async: Async.loadFromSource Async.users
                  }
                  [ HP.placeholder "Search users..."
                  , HP.id_ "loading-users"
                  ]
                )
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
              , HH.slot_ _singleLocation 9
                ( TA.component
                  { runSelect: const <<< Just
                  , runRemove: const (const Nothing)
                  , runFilterFuzzy: identity
                  , runFilterItems: const
                  }
                )
                { items: NotAsked
                , insertable: TA.NotInsertable
                , keepOpen: false
                , debounceTime: Nothing
                , async: Nothing
                , itemToObject: Async.locationToObject
                , disabled: false
                , render: TA.renderHeaderSearchDropdown
                  "All Locations"
                  "All Locations"
                  (HH.text <<< _.name <<< unwrap)
                  (HH.span_ <<< IC.boldMatches "name")
                }
              ]
            ]
          ]
        , content
          [ Card.card
            [ HP.class_ $ HH.ClassName "flex-1" ]
            [ HH.h3
              [ HP.classes Format.captionClasses ]
              [ HH.text "Searchable Dropdown in a Toolbar (e.g. for filtering)" ]
            , HH.slot_ _singleLocation 10
              ( TA.component
                { runSelect: const <<< Just
                , runRemove: const (const Nothing)
                , runFilterFuzzy: identity
                , runFilterItems: const
                }
              )
              { items: NotAsked
              , insertable: TA.NotInsertable
              , keepOpen: false
              , debounceTime: Nothing
              , async: Nothing
              , itemToObject: Async.locationToObject
              , disabled: false
              , render: TA.renderToolbarSearchDropdown
                "All Locations"
                "All Locations"
                (HH.text <<< _.name <<< unwrap)
                (HH.span_ <<< IC.boldMatches "name")
              }
            ]
          ]
        ]
      ]
    ]
