module UIGuide.Components.TextFields where

import Prelude

import Ocelot.Block.FormControl as FormControl
import Ocelot.Components.Typeahead as TA
import Ocelot.Core.Typeahead as TACore
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff.Timer (TIMER)
import DOM (DOM)
import Data.Either.Nested (Either2)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
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
  | HandleTypeahead Unit (TACore.Message Query Async.Todo) a
  | HandleSyncTypeahead (TACore.Message Query String) a

----------
-- Child paths

type ChildSlot = Either2 Unit Unit
type ChildQuery eff m =
  Coproduct2
    (TACore.Query Query Async.Todo Async.Err eff m)
    (TACore.Query Query String Void eff m)


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
    eval (HandleTypeahead slot m next) = pure next -- case m of
      --  TACore.RequestData syncMethod -> do
      --    res <- H.liftAff $ Async.load syncMethod
      --    case TACore.maybeReplaceItems res syncMethod of
      --      Nothing -> pure next
      --      (Just newSync) -> do
      --        _ <- H.query' CP.cp1 slot $ H.action $ TACore.FulfillRequest newSync
      --        pure next
      --
      --  -- Ignore other messages
      --  _ -> pure next


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
          ( HH.slot' CP.cp2 unit TACore.component
              (TA.defMulti
                [ HP.placeholder "Search developers...", HP.id_ "devs" ]
                containerData
                TA.renderItemString)
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
          ( HH.slot' CP.cp1 unit TACore.component
              (TA.defAsyncMulti
                [ HP.placeholder "Search developers asynchronously...", HP.id_ "devs-async" ]
                (\_ -> Async.loadFromSource Async.todos)
                Async.renderItemTodo)
              (HE.input $ HandleTypeahead unit)
          )
        ]
      ]
  ]
