module UIGuide.Components.TextFields where

import Prelude

import CN.UI.Components.Dropdown as Dropdown
import CN.UI.Block.FormControl as FormControl
import CN.UI.Components.Typeahead as Typeahead
import CN.UI.Core.Typeahead as TypeaheadCore
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff.Timer (TIMER)
import DOM (DOM)
import Data.Either.Nested (Either3)
import Data.Functor.Coproduct.Nested (Coproduct3)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax (AJAX)
import UIGuide.Utilities.Async as Async
import UIGuide.Block.Documentation as Documentation
import UIGuide.Block.Component as Component


----------
-- Component Types

type State
  = Unit

data Query a
  = NoOp a
  | HandleTypeahead Unit (TypeaheadCore.TypeaheadMessage Query Async.Todo (Async.Source Async.Todo) Async.Err) a
  | HandleSyncTypeahead (TypeaheadCore.TypeaheadSyncMessage Query String) a
  | HandleDropdown (Dropdown.DropdownMessage TestRecord) a

----------
-- Child paths

type ChildSlot = Either3 Unit Unit Unit
type ChildQuery eff m =
  Coproduct3
    (TypeaheadCore.TypeaheadQuery Query Async.Todo (Async.Source Async.Todo) Async.Err eff m)
    (Dropdown.Query TestRecord)
    (TypeaheadCore.TypeaheadQuery Query String Void Void eff m)


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
    render _ = cnDocumentationBlocks

    eval :: Query ~> H.ParentDSL State Query (ChildQuery (Effects eff) m) ChildSlot Void m
    eval (NoOp next) = pure next

    -- No messages necessary to handle, really.
    eval (HandleSyncTypeahead m next) = pure next

    -- Responsible for fetching data based on source and returning it to the component.
    -- Done asynchronously so data can load in the background.
    eval (HandleTypeahead slot m next) = case m of
      TypeaheadCore.RequestData syncMethod -> do
        res <- H.liftAff $ Async.load syncMethod
        case TypeaheadCore.maybeReplaceItems res syncMethod of
          Nothing -> pure next
          (Just newSync) -> do
            _ <- H.query' CP.cp1 slot $ H.action $ TypeaheadCore.FulfillRequest newSync
            pure next

      -- Ignore other messages
      _ -> pure next

    eval (HandleDropdown m next) = pure next


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


----------
-- HTML

css :: ∀ t0 t1. String -> H.IProp ( "class" :: String | t0 ) t1
css = HP.class_ <<< HH.ClassName

cnDocumentationBlocks :: ∀ eff m
  . MonadAff (Effects eff) m
 => H.ParentHTML Query (ChildQuery (Effects eff) m) ChildSlot m
cnDocumentationBlocks =
  HH.div_
  [ Documentation.documentation
      { header: "Typeaheads"
      , subheader: "Use string input to search pre-determined entries."
      }
      [ Component.component
        { title: "Synchronous Typeahead" }
        [ FormControl.formControl
          { label: "Developers"
          , helpText: Just "There are lots of developers to choose from."
          , valid: Nothing
          , inputId: "devs"
          }
          ( HH.slot' CP.cp3 unit TypeaheadCore.component
              (Typeahead.defMulti
                [ HP.placeholder "Search developers...", HP.id_ "devs" ]
                containerData
                Typeahead.renderItemString)
              (HE.input HandleSyncTypeahead)
          )
        ]
      , Component.component
        { title: "Continuous Asynchronous Typeahead" }
        [ FormControl.formControl
          { label: "Developers"
          , helpText: Just "There are lots of developers to choose from."
          , valid: Nothing
          , inputId: "devs-async"
          }
          ( HH.slot' CP.cp1 unit TypeaheadCore.component
              (Typeahead.defContAsyncMulti
                [ HP.placeholder "Search developers asynchronously...", HP.id_ "devs-async" ]
                Async.todos
                Async.renderItemTodo)
              (HE.input $ HandleTypeahead unit)
          )
        ]
      ]
  , Documentation.documentation
      { header: "Dropdowns"
      , subheader: "Select from pre-determined entries."
      }
      [ Component.component
        { title: "Dropdown" }
        [ FormControl.formControl
          { label: "Platforms"
          , helpText: Just "There are lots of platforms to choose from."
          , valid: Nothing
          , inputId: ""
          }
          dropdownSingleSlot
        ]
      , Component.component
        { title: "Dropdown" }
        [ FormControl.formControl
          { label: "Lorem Ipsum"
          , helpText: Just "Dolor sit amet consectectuer."
          , valid: Nothing
          , inputId: ""
          }
          dropdownMultiSlot
        ]
      ]
  ]


dropdownSingleSlot =
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

dropdownMultiSlot =
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
