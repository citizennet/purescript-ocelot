module UIGuide.Components.TextFields where

import Prelude

import UIGuide.Blocks.Sidebar as Sidebar
import UIGuide.Utilities.Async as Async

import CN.UI.Block.Button as Button
import CN.UI.Components.Dropdown as Dropdown
import CN.UI.Components.Typeahead as Typeahead
import CN.UI.Core.Typeahead as Typeahead

import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Aff.Console (logShow)
import Control.Monad.Eff.Timer (TIMER)

import Network.HTTP.Affjax (get, AJAX)

import Data.Either.Nested (Either2)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (Tuple)

import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import UIGuide.Utilities.Effects (MyEffects)


----------
-- Component Types

type State
  = Unit

data Query a
  = NoOp a
  | HandleTypeahead TypeaheadSlot (Typeahead.TypeaheadMessage Query Async.Item (Async.Source Async.Item) Async.Err) a
  | HandleDropdown (Dropdown.DropdownMessage TestRecord) a


----------
-- Child paths

type ChildSlot = Either2 TypeaheadSlot Unit
type ChildQuery eff m =
  Coproduct2
    (Typeahead.TypeaheadQuery Query Async.Item (Async.Source Async.Item) Async.Err (MyEffects eff) m)
    (Dropdown.Query TestRecord)

data TypeaheadSlot
  = TypeaheadTodos
  | TypeaheadUsers
derive instance eqTypeaheadSlot :: Eq TypeaheadSlot
derive instance ordTypeaheadSlot :: Ord TypeaheadSlot

----------
-- Convenience types

type HTML eff m = H.ParentHTML Query (ChildQuery eff m) ChildSlot m

----------
-- Component definition

component :: ∀ m eff
  . MonadAff ( MyEffects eff ) m
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
    --  render :: State -> HTML eff m
    render st = container Sidebar.cnNavSections cnDocumentationBlocks

    eval :: Query ~> H.ParentDSL State Query (ChildQuery eff m) ChildSlot Void m
    eval (NoOp next) = pure next

    eval (HandleTypeahead slot m next) = case m of
      Typeahead.RequestData source -> do
        res <- H.liftAff $ Async.load source
        _ <- H.query' CP.cp1 slot $ H.action $ Typeahead.FulfillRequest $ replaceItems res source
        pure next

      -- Ignore other messages
      _ -> pure next

    eval (HandleDropdown m next) = pure next <* case m of
      Dropdown.ItemRemoved item ->
        H.liftAff $ logShow ((unwrap >>> _.name) item <> " was removed")
      Dropdown.ItemSelected item ->
        H.liftAff $ logShow ((unwrap >>> _.name) item <> " was selected")

    -- Helper to replace items dependent on sync types
    replaceItems Nothing v = v
    replaceItems (Just _) s@(Typeahead.Sync _) = s
    replaceItems (Just d) (Typeahead.Async src _) = Typeahead.Async src d
    replaceItems (Just d) (Typeahead.ContinuousAsync srch src _) = Typeahead.ContinuousAsync srch src d


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

--  container :: ∀ eff m
--    . MonadAff ( MyEffects eff ) m
--   => Array (Tuple String (Array (Tuple String String)))
--   -> Array (HTML eff m)
--   -> HTML eff m
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

--  cnDocumentationBlocks :: ∀ eff m
--    . MonadAff ( MyEffects eff ) m
--   => Array (HTML eff m)
cnDocumentationBlocks = typeaheadBlockTodos <> typeaheadBlockUsers <> dropdownBlock <> buttonBlock

--  buttonBlock :: ∀ eff m
--    . MonadAff ( MyEffects eff ) m
--   => Array (HTML eff m)
buttonBlock = documentationBlock
  "Button"
  "Some button shit"
  (HH.div_
    [ Button.button_
      { type_: Button.Primary }
      [ HH.text "Submit" ]
    , HH.span
      [ HP.class_ (HH.ClassName "ml-4") ]
      [ Button.button
        { type_: Button.Default }
        []
        [ HH.text "Cancel" ]
      ]
    ]
  )

--  typeaheadBlockTodos :: ∀ eff m
--    . MonadAff ( MyEffects eff ) m
--   => Array (HTML eff m)
typeaheadBlockTodos = documentationBlock
  "Typeahead"
  "Uses string input to search pre-determined entries."
  ( componentBlock "No configuration set." slot )
  where
    slot =
      HH.slot' CP.cp1 TypeaheadTodos Typeahead.component ( Typeahead.testAsyncMulti' Async.todos ) (HE.input $ HandleTypeahead TypeaheadTodos)

--  typeaheadBlockUsers :: ∀ eff m
--    . MonadAff ( MyEffects eff ) m
--   => Array (HTML eff m)
typeaheadBlockUsers = documentationBlock
  "Typeahead"
  "Uses string input to search pre-determined entries."
  ( componentBlock "No configuration set." slot )
  where
    slot =
      HH.slot' CP.cp1 TypeaheadUsers Typeahead.component ( Typeahead.testAsyncMulti' Async.users ) (HE.input $ HandleTypeahead TypeaheadUsers)

--  dropdownBlock :: ∀ eff m
--    . MonadAff ( MyEffects eff ) m
--   => Array (HTML eff m)
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
    mkConfig :: ∀ i p. String -> H.HTML i p
    mkConfig str = HH.p_ [ HH.text str ]
