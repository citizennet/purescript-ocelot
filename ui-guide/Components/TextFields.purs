module UIGuide.Components.TextFields where

import Prelude

import CN.UI.Block.NavigationTab as NavigationTab
import CN.UI.Components.Dropdown as Dropdown
import CN.UI.Components.Typeahead as Typeahead
import CN.UI.Core.Typeahead as Typeahead
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Aff.Console (CONSOLE, logShow)
import Control.Monad.Eff.Timer (TIMER)
import DOM (DOM)
import Data.Either.Nested (Either3)
import Data.Functor.Coproduct.Nested (Coproduct3)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.StrMap (StrMap)
import Data.StrMap as StrMap
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax (AJAX)
import UIGuide.Block.Component as Component
import UIGuide.Block.Documentation (documentation)
import UIGuide.Utilities.Async as Async

----------
-- Component Types

type State
  = Unit


data Query a
  = NoOp a
  | HandleTypeahead Unit (Typeahead.TypeaheadMessage Query Async.Todo (Async.Source Async.Todo) Async.Err) a
  | HandleSyncTypeahead (Typeahead.TypeaheadSyncMessage Query String) a
  | HandleDropdown (Dropdown.DropdownMessage TestRecord) a

----------
-- Child paths

type ChildSlot = Either3 Unit Unit Unit
type ChildQuery eff m =
  Coproduct3
    (Typeahead.TypeaheadQuery Query Async.Todo (Async.Source Async.Todo) Async.Err eff m)
    (Dropdown.Query TestRecord)
    (Typeahead.TypeaheadQuery Query String Void Void eff m)


----------
-- Component definition

-- NOTE: Uses the same effects but does not compose with typeahead effects. Written out again from scratch.
type Effects eff = ( avar :: AVAR, dom :: DOM, ajax :: AJAX, timer :: TIMER, console :: CONSOLE | eff )

component :: ∀ eff m
  . MonadAff (Effects eff) m
 => H.Component HH.HTML Query Unit Void m
component =
  H.parentComponent
  { initialState: const unit
  , render
  , eval
  , receiver: const Nothing
  }
  where
    -- For the sake of testing and visual demonstration, we'll just render
    -- out a bunch of selection variants in respective slots
    render :: State -> H.ParentHTML Query (ChildQuery (Effects eff) m) ChildSlot m
    render st = HH.div_ cnDocumentationBlocks

    eval :: Query ~> H.ParentDSL State Query (ChildQuery (Effects eff) m) ChildSlot Void m
    eval (NoOp next) = pure next

    -- No messages necessary to handle, really.
    eval (HandleSyncTypeahead m next) = pure next

    -- Responsible for fetching data based on source and returning it to the component.
    -- Done asynchronously so data can load in the background.
    eval (HandleTypeahead slot m next) = case m of
      Typeahead.RequestData syncMethod -> do
        _ <- H.fork do
          res <- H.liftAff $ Async.load syncMethod
          case Typeahead.maybeReplaceItems res syncMethod of
            Nothing -> pure next
            (Just newSync) -> do
              _ <- H.query' CP.cp1 slot $ H.action $ Typeahead.FulfillRequest newSync
              pure next
        pure next

      -- Ignore other messages
      _ -> pure next

    eval (HandleDropdown m next) = pure next <* case m of
      Dropdown.ItemRemoved item ->
        H.liftAff $ logShow ((unwrap >>> _.name) item <> " was removed")
      Dropdown.ItemSelected item ->
        H.liftAff $ logShow ((unwrap >>> _.name) item <> " was selected")


----------
-- Sample data

newtype TestRecord = TestRecord
  { name :: String
  , id :: Int
  }

instance eqTestRecord :: Eq TestRecord where
  eq (TestRecord { id: id'' }) (TestRecord { id: id' }) = id'' == id'

derive instance newtypeTestRecord :: Newtype TestRecord _

dropdownData :: Array TestRecord
dropdownData =
  [ TestRecord { name: "Chris", id: 0 }
  , TestRecord { name: "Dave", id: 1 }
  , TestRecord { name: "Thomas", id: 2 }
  , TestRecord { name: "Forest", id: 3 }
  ]

containerData :: Array String
containerData =
  [ "Instagram"
  , "Facebook"
  , "Twitter"
  , "Pinterest"
  , "Snapchat"
  , "YouTube"
  , "Reddit"
  , "Voat"
  , "Discord"
  , "4Chan"
  , "8Chan"
  , "Digg"
  , "Myspace"
  , "Friendster"
  ]

tabs :: Array (NavigationTab.Tab Boolean)
tabs =
  [ { name: "Accounts & Spend", link: "#", page: true }
  , { name: "Automatic Optimization", link: "#", page: false }
  , { name: "Creative", link: "#", page: false }
  ]

tabConfig :: NavigationTab.TabConfig Boolean
tabConfig =
  { tabs: tabs
  , activePage: true
  }


----------
-- HTML

css :: ∀ t0 t1. String -> H.IProp ( "class" :: String | t0 ) t1
css = HP.class_ <<< HH.ClassName

cnDocumentationBlocks :: ∀ eff m. MonadAff (Effects eff) m => Array (H.ParentHTML Query (ChildQuery (Effects eff) m) ChildSlot m)
cnDocumentationBlocks =
  tabsBlock
  <> typeaheadBlockStrings
  <> typeaheadBlockTodos
  <> dropdownBlock

tabsBlock
  :: ∀ eff m
  . MonadAff (Effects eff) m
  => Array (H.ParentHTML Query (ChildQuery (Effects eff) m) ChildSlot m)
tabsBlock =
  [ documentation
      { header: "Tabs"
      , subheader: "Tabs for navigating, eg. between form pages"
      }
      [ Component.component
          { title: "Tabs" }
          [ NavigationTab.navigationTabs tabConfig ]
      ]
  ]

typeaheadBlockStrings :: ∀ eff m
  . MonadAff (Effects eff) m
 => Array (H.ParentHTML Query (ChildQuery (Effects eff) m) ChildSlot m)
typeaheadBlockStrings = documentationBlock
  "Synchronous Typeahead"
  "Uses string input to search pre-determined entries."
  ( componentBlock "Set to sync." slot )
  where
    slot =
      HH.slot'
        CP.cp3
        unit
        Typeahead.component
        ( Typeahead.defaultMulti
            containerData
            (StrMap.singleton "id")
            (Typeahead.defaultContainerRow $ Typeahead.boldMatches "id")
            (Typeahead.defaultSelectionRow $ HH.text)
        )
        (HE.input $ HandleSyncTypeahead )

typeaheadBlockTodos :: ∀ eff m
  . MonadAff (Effects eff) m
 => Array (H.ParentHTML Query (ChildQuery (Effects eff) m) ChildSlot m)
typeaheadBlockTodos = documentationBlock
  "Continuous Async Typeahead"
  "Uses string input to search pre-determined entries; fetches data."
  ( componentBlock "Set to continuous async." slot )
  where
    slot =
      HH.slot'
        CP.cp1
        unit
        Typeahead.component
        ( Typeahead.defaultContAsyncMulti
            Async.todos
            Async.todoToStrMap
            Async.todoRenderFuzzy
            Async.todoRenderItem
        )
        (HE.input $ HandleTypeahead unit)

dropdownBlock :: ∀ eff m
  . MonadAff ( Effects eff ) m
 => Array (H.ParentHTML Query (ChildQuery (Effects eff) m) ChildSlot m)
dropdownBlock = documentationBlock
  "Dropdown"
  "Select from a list."
  $ HH.div_
    [ componentBlock "Single select configuration." singleSlot
    , componentBlock "Multi select configuration." multiSlot
    ]
  where
    singleSlot =
      ( HH.slot'
          CP.cp2
          unit
          Dropdown.component
          { items: dropdownData
          , itemHTML: \i -> [ HH.text $ (_.name <<< unwrap) i ]
          , selection: Dropdown.Single Nothing
          , placeholder: "Select a dev..."
          , title: "Single Selection"
          , helpText: "Some useful help text can go here."
          }
          (HE.input HandleDropdown)
      )
    multiSlot =
      ( HH.slot'
          CP.cp2
          unit
          Dropdown.component
          { items: dropdownData
          , itemHTML: \i -> [ HH.text $ (_.name <<< unwrap) i ]
          , selection: Dropdown.Multi []
          , placeholder: "Select some devs..."
          , title: "Multi Selection"
          , helpText: "Some useful help text can go here."
          }
          (HE.input HandleDropdown)
      )

documentationBlock :: ∀ i p
  . String
 -> String
 -> H.HTML i p
 -> Array (H.HTML i p)
documentationBlock title description block =
  [ HH.h1_ [ HH.text title ]
  , HH.div [ css "text-xl text-grey-dark mb-4" ] [ HH.text description ]
  , block ]

componentBlock :: ∀ i p
  . String
 -> H.HTML i p
 -> H.HTML i p
componentBlock config slot =
  HH.div
  [ css "rounded border-2 border-grey-light mb-8 bg-white" ]
  [ HH.div [ css "border-b-2 border-grey-light p-4" ] [ mkConfig config ]
  , HH.div [ css "p-4 pb-8 bg-grey-lightest" ] [ slot ]
  ]
  where
    mkConfig :: String -> H.HTML i p
    mkConfig str = HH.p_ [ HH.text str ]
