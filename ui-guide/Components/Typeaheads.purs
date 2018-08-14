module UIGuide.Component.Typeaheads where

import Prelude

import Data.Array (head, take)
import Data.Either.Nested (Either2, Either5)
import Data.Functor.Coproduct.Nested (Coproduct3, Coproduct5)
import Data.Fuzzy (Fuzzy(..))
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..))
import Ocelot.Block.Card as Card
import Ocelot.Block.FormField as FormField
import Ocelot.Block.Format as Format
import Ocelot.Block.ItemContainer (boldMatches) as IC
import Ocelot.Component.Typeahead as TA
import Ocelot.Component.Typeahead.Render as TA
import Ocelot.HTML.Properties (css)
import Select as Select
import UIGuide.Block.Backdrop as Backdrop
import UIGuide.Block.Documentation as Documentation
import UIGuide.Utility.Async as Async

----------
-- Component Types

type State = Unit

data Query a
  = NoOp a
  | SingleUser Int (TA.Message Query Maybe Async.User) a
  | MultiUser Int (TA.Message Query Array Async.User) a
  | SingleLocation Int (TA.Message Query Maybe Async.Location) a
  | MultiLocation Int (TA.Message Query Array Async.Location) a
  | Initialize a

----------
-- Child paths

type ChildSlot = Either5 Int Int Int Int Unit
type ChildQuery =
  Coproduct5
    (TA.Query Query Maybe Async.User)
    (TA.Query Query Array Async.User)
    (TA.Query Query Maybe Async.Location)
    (TA.Query Query Array Async.Location)
    Query


----------
-- Component definition

component :: ∀ m
  . MonadAff m
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
      -> H.ParentHTML Query ChildQuery ChildSlot m
    render _ = cnDocumentationBlocks

    eval
      :: Query
      ~> H.ParentDSL State Query ChildQuery ChildSlot Void m
    eval (NoOp next) = pure next


    -- No longer necessary to fetch data. Treat it just like a Sync typeahead.
    eval (SingleUser slot m next) = pure next
    eval (MultiUser slot m next) = pure next
    eval (SingleLocation slot m next) = pure next
    eval (MultiLocation slot m next) = pure next

    eval (Initialize next) = do
      --  _ <- H.queryAll' CP.cp1 $ H.action $ TA.ReplaceItems Loading
      --  _ <- H.queryAll' CP.cp2 $ H.action $ TA.ReplaceItems Loading
      --  remoteLocations <- H.liftAff $ Async.loadFromSource Async.locations ""
      --  _ <- case remoteLocations of
      --    items@(Success _) -> do
      --      _ <- H.queryAll' CP.cp1 $ H.action $ TA.ReplaceItems items
      --      pure unit
      --    otherwise -> pure unit
      --  remoteUsers <- H.liftAff $ Async.loadFromSource Async.users ""
      --  _ <- case remoteUsers of
      --    items@(Success _) -> do
      --      _ <- H.queryAll' CP.cp2 $ H.action $ TA.ReplaceItems items
      --      pure unit
      --    otherwise -> pure unit
      --  selectedLocations <- H.liftAff $ Async.loadFromSource Async.locations "an"
      --  _ <- case selectedLocations of
      --    Success xs -> do
      --      _ <- H.query' CP.cp1 1
      --        $ H.action
      --        $ TA.ReplaceSelected
      --        $ head xs
      --      _ <- H.query' CP.cp1 3
      --        $ H.action
      --        $ TA.ReplaceSelected
      --        $ take 4 xs
      --      _ <- H.query' CP.cp1 5
      --        $ H.action
      --        $ TA.ReplaceSelected
      --        $ head xs
      --      pure unit
      --    otherwise -> pure unit
      --  selectedUsers <- H.liftAff $ Async.loadFromSource Async.users "an"
      --  _ <- case selectedUsers of
      --    Success xs -> do
      --      _ <- H.query' CP.cp2 1
      --        $ H.action
      --        $ TA.ReplaceSelected
      --        $ head xs
      --      _ <- H.query' CP.cp2 3
      --        $ H.action
      --        $ TA.ReplaceSelected
      --        $ take 4 xs
      --      _ <- H.query' CP.cp2 5
      --        $ H.action
      --        $ TA.ReplaceSelected
      --        $ take 4 xs
      --      pure next
      --    otherwise -> pure next
      --  _ <- H.query' CP.cp1 6 $ H.action $ TA.ReplaceItems $ Failure ""
      --  _ <- H.query' CP.cp2 6 $ H.action $ TA.ReplaceItems $ Failure ""
      --  _ <- H.query' CP.cp1 7 $ H.action $ TA.ReplaceItems Loading
      --  _ <- H.query' CP.cp2 7 $ H.action $ TA.ReplaceItems Loading
      pure next

----------
-- HTML

content :: ∀ p i. Array (HH.HTML p (i Unit)) -> HH.HTML p (i Unit)
content = Backdrop.content [ css "flex" ]

cnDocumentationBlocks :: ∀ m
  . MonadAff m
 => H.ParentHTML Query ChildQuery ChildSlot m
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
              , helpText: Just "Search your favorite destination."
              , error: Nothing
              , inputId: "location"
              }
              [ HH.slot' CP.cp3 0 (TA.single (\_ _ -> HH.div_ []))
                { items: NotAsked
                , insertable: TA.NotInsertable
                , keepOpen: true
                , itemToObject: Async.locationToObject
                , asyncConfig: Just
                    { debounceTime: Milliseconds 250.0
                    , fetchItems: Async.loadFromSource Async.locations
                    }
                }
                ( HE.input $ SingleLocation 0 )
              ]
            , HH.h3
              [ HP.classes Format.captionClasses ]
              [ HH.text "Standard Hydrated" ]
            , FormField.field_
              { label: HH.text "Locations"
              , helpText: Just "Search your favorite destination."
              , error: Nothing
              , inputId: "location-hydrated"
              }
              []
              --  [ HH.slot' CP.cp1 1 TA.single
              --    (TA.defAsyncSingle
              --      [ HP.placeholder "Search locations..."
              --      , HP.id_ "location-hydrated"
              --      ]
              --      ( Async.loadFromSource Async.locations )
              --      Async.renderItemLocation
              --    )
              --    ( HE.input $ HandleTypeaheadLocation 1 )
              --  ]
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
              , helpText: Just "Search your favorite companion."
              , error: Nothing
              , inputId: "user"
              }
              []
              --  [ HH.slot' CP.cp2 0 TA.single
              --    (TA.defAsyncSingle
              --      [ HP.placeholder "Search users..."
              --      , HP.id_ "user"
              --      ]
              --      ( Async.loadFromSource Async.users )
              --      Async.renderItemUser
              --    )
              --    ( HE.input $ HandleTypeaheadUser 0 )
              --  ]
            , HH.h3
              [ HP.classes Format.captionClasses ]
              [ HH.text "Custom Render Hydrated" ]
            , FormField.field_
              { label: HH.text "Users"
              , helpText: Just "Search your favorite companion."
              , error: Nothing
              , inputId: "user-hydrated"
              }
              []
              --  [ HH.slot' CP.cp2 1 TA.single
              --    (TA.defAsyncSingle
              --      [ HP.placeholder "Search users..."
              --      , HP.id_ "user-hydrated"
              --      ]
              --      ( Async.loadFromSource Async.users )
              --      Async.renderItemUser
              --    )
              --    ( HE.input $ HandleTypeaheadUser 1 )
              --  ]
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
              , helpText: Just "Search your top destinations."
              , error: Nothing
              , inputId: "locations"
              }
              []
              --  [ HH.slot' CP.cp1 2 TA.multi
              --    (TA.defAsyncMulti
              --      [ HP.placeholder "Search locations..."
              --      , HP.id_ "locations"
              --      ]
              --      ( Async.loadFromSource Async.locations )
              --      Async.renderItemLocation
              --    )
              --    ( HE.input $ HandleTypeaheadLocation 2 )
              --  ]
            , HH.h3
              [ HP.classes Format.captionClasses ]
              [ HH.text "Standard Hydrated" ]
            , FormField.field_
              { label: HH.text "Locations"
              , helpText: Just "Search your top destinations."
              , error: Nothing
              , inputId: "locations"
              }
              []
              --  [ HH.slot' CP.cp1 3 TA.multi
              --    (TA.defAsyncMulti
              --      [ HP.placeholder "Search locations..."
              --      , HP.id_ "locations"
              --      ]
              --      ( Async.loadFromSource Async.locations )
              --      Async.renderItemLocation
              --    )
              --    ( HE.input $ HandleTypeaheadLocation 3 )
              --  ]
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
              , helpText: Just "Search your top companions."
              , error: Nothing
              , inputId: "users"
              }
              []
              --  [ HH.slot' CP.cp2 2 TA.multi
              --    (TA.defAsyncMulti
              --      [ HP.placeholder "Search users..."
              --      , HP.id_ "users"
              --      ]
              --      ( Async.loadFromSource Async.users )
              --      Async.renderItemUser
              --    )
              --    ( HE.input $ HandleTypeaheadUser 2 )
              --  ]
            , HH.h3
              [ HP.classes Format.captionClasses ]
              [ HH.text "Custom Render Hydrated" ]
            , FormField.field_
              { label: HH.text "Users"
              , helpText: Just "Search your top companions."
              , error: Nothing
              , inputId: "users-hydrated"
              }
              []
              --  [ HH.slot' CP.cp2 3 TA.multi
              --    (TA.defAsyncMulti
              --      [ HP.placeholder "Search users..."
              --      , HP.id_ "users-hydrated"
              --      ]
              --      ( Async.loadFromSource Async.users )
              --      Async.renderItemUser
              --    )
              --    ( HE.input $ HandleTypeaheadUser 3 )
              --  ]
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
              , helpText: Just "Search your top destinations."
              , error: Nothing
              , inputId: "disabled-locations-empty"
              }
              []
              --  [ HH.slot' CP.cp1 4 TA.single
              --    (TA.defAsyncSingle
              --      [ HP.placeholder "Search locations..."
              --      , HP.id_ "disabled-locations-empty"
              --      , HP.disabled true
              --      ]
              --      ( Async.loadFromSource Async.locations )
              --      Async.renderItemLocation
              --    )
              --    ( HE.input $ HandleTypeaheadLocation 4 )
              --  ]
            , HH.h3
              [ HP.classes Format.captionClasses ]
              [ HH.text "Disabled Single Select - Hydrated" ]
            , FormField.field_
              { label: HH.text "Locations"
              , helpText: Just "Search your top destinations."
              , error: Nothing
              , inputId: "disabled-locations-hydrated"
              }
              []
              --  [ HH.slot' CP.cp1 5 TA.single
              --    (TA.defAsyncSingle
              --      [ HP.placeholder "Search locations..."
              --      , HP.id_ "disabled-locations-hydrated"
              --      , HP.disabled true
              --      ]
              --      ( Async.loadFromSource Async.locations )
              --      Async.renderItemLocation
              --    )
              --    ( HE.input $ HandleTypeaheadLocation 5 )
              --  ]
            , HH.h3
              [ HP.classes Format.captionClasses ]
              [ HH.text "Error Single Select" ]
            , FormField.field_
              { label: HH.text "Locations"
              , helpText: Just "Search your top destinations."
              , error: Nothing
              , inputId: "error-locations"
              }
              []
              --  [ HH.slot' CP.cp1 6 TA.single
              --    (TA.defAsyncSingle
              --      [ HP.placeholder "Search locations..."
              --      , HP.id_ "error-locations"
              --      ]
              --      ( Async.loadFromSource Async.locations )
              --      Async.renderItemLocation
              --    )
              --    ( HE.input $ HandleTypeaheadLocation 6 )
              --  ]
            , HH.h3
              [ HP.classes Format.captionClasses ]
              [ HH.text "Loading Single Select" ]
            , FormField.field_
              { label: HH.text "Locations"
              , helpText: Just "Search your top destinations."
              , error: Nothing
              , inputId: "loading-locations"
              }
              []
              --  [ HH.slot' CP.cp1 7 TA.single
              --    (TA.defAsyncSingle
              --      [ HP.placeholder "Search locations..."
              --      , HP.id_ "loading-locations"
              --      ]
              --      ( Async.loadFromSource Async.locations )
              --      Async.renderItemLocation
              --    )
              --    ( HE.input $ HandleTypeaheadLocation 7 )
              --  ]
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
              , helpText: Just "Search your top companions."
              , error: Nothing
              , inputId: "disabled-users-empty"
              }
              []
              --  [ HH.slot' CP.cp2 4 TA.multi
              --    (TA.defAsyncMulti
              --      [ HP.placeholder "Search Users..."
              --      , HP.id_ "disabled-users-empty"
              --      , HP.disabled true
              --      ]
              --      ( Async.loadFromSource Async.users )
              --      Async.renderItemUser
              --    )
              --    ( HE.input $ HandleTypeaheadUser 4 )
              --  ]
            , HH.h3
              [ HP.classes Format.captionClasses ]
              [ HH.text "Disabled Multi Select - Hydrated" ]
            , FormField.field_
              { label: HH.text "Users"
              , helpText: Just "Search your top companions."
              , error: Nothing
              , inputId: "disabled-users-hydrated"
              }
              []
              --  [ HH.slot' CP.cp2 5 TA.multi
              --    (TA.defAsyncMulti
              --      [ HP.placeholder "Search Users..."
              --      , HP.id_ "disabled-users-hydrated"
              --      , HP.disabled true
              --      ]
              --      ( Async.loadFromSource Async.users )
              --      Async.renderItemUser
              --    )
              --    ( HE.input $ HandleTypeaheadUser 5 )
              --  ]
            , HH.h3
              [ HP.classes Format.captionClasses ]
              [ HH.text "Error Multi Select" ]
            , FormField.field_
              { label: HH.text "Users"
              , helpText: Just "Search your top companions."
              , error: Nothing
              , inputId: "error-users"
              }
              []
              --  [ HH.slot' CP.cp2 6 TA.multi
              --    (TA.defAsyncMulti
              --      [ HP.placeholder "Search users..."
              --      , HP.id_ "error-users"
              --      ]
              --      ( Async.loadFromSource Async.users )
              --      Async.renderItemUser
              --    )
              --    ( HE.input $ HandleTypeaheadUser 6 )
              --  ]
            , HH.h3
              [ HP.classes Format.captionClasses ]
              [ HH.text "Loading Multi Select" ]
            , FormField.field_
              { label: HH.text "Users"
              , helpText: Just "Search your top companions."
              , error: Nothing
              , inputId: "loading-users"
              }
              []
              --  [ HH.slot' CP.cp2 7 TA.multi
              --    (TA.defAsyncSingle
              --      [ HP.placeholder "Search users..."
              --      , HP.id_ "loading-users"
              --      ]
              --      ( Async.loadFromSource Async.users )
              --      Async.renderItemUser
              --    )
              --    ( HE.input $ HandleTypeaheadUser 7 )
              --  ]
            ]
          ]
        ]
      ]
    ]


--  renderSingleLocation
--    :: ∀ m
--     . MonadAff m
--    => TA.State Maybe Async.Location m
--    -> Select.State (Fuzzy Async.Location)
--    -> Select.ComponentHTML (TA.Query Query Maybe Async.Location) (Fuzzy Async.Location)
--  renderSingleLocation =
--    TA.renderSingle
--      [ HP.placeholder "Search locations..."
--      , HP.id_ "location"
--      ]
--      (HH.span_ <<< IC.boldMatches "name")
--      (TA.defRenderContainer $ HH.span_ <<< IC.boldMatches "name")
