module UIGuide.Components.TextFields where

import Prelude

import UIGuide.Blocks.Sidebar as Sidebar
import UIGuide.Utilities.Async as Async

import CN.UI.Components.Dropdown as Dropdown
import CN.UI.Components.Typeahead (testAsyncMulti') as Typeahead
import CN.UI.Core.Typeahead as Typeahead
import Control.Monad.Aff.Console (logShow)
import Data.Either.Nested (Either2)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.Maybe (Maybe(Nothing))
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (Tuple)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Select.Effects (FX)


----------
-- Component Types

type State
  = Unit

data Query a
  = NoOp a
  | HandleDropdown (Dropdown.DropdownMessage TestRecord) a
  | HandleTypeahead (Typeahead.TypeaheadMessage Query Async.Todo) a


----------
-- Child paths

type ChildQuery e = Coproduct2 (Typeahead.TypeaheadQuery Query Async.Todo e) (Dropdown.Query TestRecord)
type ChildSlot = Either2 Unit Unit


----------
-- Convenience types

type HTML e = H.ParentHTML Query (ChildQuery e) ChildSlot (FX e)

----------
-- Component definition

component :: ∀ e. H.Component HH.HTML Query Unit Void (FX e)
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
    render :: State -> HTML e
    render st = container Sidebar.cnNavSections cnDocumentationBlocks

    eval :: Query ~> H.ParentDSL State Query (ChildQuery e) ChildSlot Void (FX e)
    eval (NoOp next) = pure next

    eval (HandleTypeahead m next) = case m of
      Typeahead.RequestData (Typeahead.Async src _) -> do
         -- Not using SRC here but should!
         todos <- H.liftAff Async.fetchTodos
         _ <- H.query' CP.cp1 unit $ H.action ( Typeahead.FulfillRequest $ Typeahead.Async src todos )
         pure next
      Typeahead.RequestData (Typeahead.ContinuousAsync src _) -> do
         -- Not using SRC or STR here but should!
         todos <- H.liftAff Async.fetchTodos
         H.liftAff $ logShow todos
         _ <- H.query' CP.cp1 unit $ H.action ( Typeahead.FulfillRequest $ Typeahead.ContinuousAsync src todos )
         pure next
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
  , "Twitter" ]


----------
-- HTML

css :: ∀ t0 t1. String -> H.IProp ( "class" :: String | t0 ) t1
css = HP.class_ <<< HH.ClassName

container :: ∀ e
  . Array (Tuple String (Array (Tuple String String)))
 -> Array (HTML e)
 -> HTML e
container navs blocks =
  HH.body
  [ css "font-sans font-normal text-black leading-normal" ]
  [ HH.div
    [ css "min-h-screen" ]
    [ Sidebar.sidebar navs
    , innerContainer "CitizenNet UI Guide" blocks
    ]
  ]

innerContainer :: ∀ i p. String -> Array (H.HTML i p) -> H.HTML i p
innerContainer title blocks =
  HH.div
  [ css "md:ml-80" ]
  [ HH.div
    [ css "fixed w-full z-20" ]
    [ HH.div
      [ css "pin-t bg-white md:hidden relative border-b border-grey-light h-12 flex items-center" ]
      [ HH.a
        [ css "mx-auto inline-flex items-center"
        , HP.href "#" ]
        [ HH.text title ]
      ]
    ]
  , HH.div
    [ css "px-6 pb-8 pt-20 md:pt-16 w-full max-w-lg mx-auto" ]
    blocks
  ]

cnDocumentationBlocks :: ∀ e. Array (HTML e)
cnDocumentationBlocks = typeaheadBlock <> dropdownBlock

typeaheadBlock :: ∀ e. Array (HTML e)
typeaheadBlock = documentationBlock
  "Typeahead"
  "Uses string input to search pre-determined entries."
  ( componentBlock "No configuration set." slot )
  where
    slot =
      HH.slot' CP.cp1 unit Typeahead.component ( Typeahead.testAsyncMulti' "GET ME SOME TODOS" ) (HE.input HandleTypeahead)

dropdownBlock :: ∀ e. Array (HTML e)
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


documentationBlock :: ∀ i p. String -> String -> H.HTML i p -> Array (H.HTML i p)
documentationBlock title description block =
  [ HH.h1_ [ HH.text title ]
  , HH.div [ css "text-xl text-grey-dark mb-4" ] [ HH.text description ]
  , block ]

componentBlock :: ∀ e. String -> HTML e -> HTML e
componentBlock config slot =
  HH.div
  [ css "rounded border-2 border-grey-light mb-8 bg-white" ]
  [ HH.div [ css "border-b-2 border-grey-light p-4" ] [ mkConfig config ]
  , HH.div [ css "p-4 pb-8 bg-grey-lightest" ] [ slot ]
  ]
  where
    mkConfig :: ∀ i p. String -> H.HTML i p
    mkConfig str = HH.p_ [ HH.text str ]
