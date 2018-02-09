module UIGuide.Components.TextFields where

import Prelude

import CN.UI.Block.Button as Button
import CN.UI.Block.FormControl as FormControl
import CN.UI.Block.Input as Input
import CN.UI.Block.NavigationTab as NavigationTab
import CN.UI.Block.Radio as Radio
import CN.UI.Block.Toggle as Toggle
import CN.UI.Components.Dropdown as Dropdown
import CN.UI.Components.Typeahead (defaultMulti', defaultAsyncMulti', defaultContAsyncMulti') as Typeahead
import CN.UI.Core.Typeahead (SyncMethod(..), TypeaheadMessage(..), TypeaheadSyncMessage, TypeaheadQuery(..), component) as Typeahead
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Aff.Console (logShow, CONSOLE)
import Control.Monad.Eff.Timer (TIMER)
import DOM (DOM)
import Data.Either.Nested (Either3)
import Data.Functor.Coproduct.Nested (Coproduct3)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (Tuple)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax (AJAX)
import UIGuide.Blocks.Sidebar as Sidebar
import UIGuide.Utilities.Async as Async

----------
-- Component Types

type State
  = Unit


data Query a
  = NoOp a
  | HandleTypeahead TypeaheadSlot (Typeahead.TypeaheadMessage Query Async.Item (Async.Source Async.Item) Async.Err) a
  | HandleSyncTypeahead (Typeahead.TypeaheadSyncMessage Query String) a
  | HandleDropdown (Dropdown.DropdownMessage TestRecord) a


----------
-- Child paths

type ChildSlot = Either3 TypeaheadSlot Unit SyncTypeaheadSlot
type ChildQuery eff m =
  Coproduct3
    (Typeahead.TypeaheadQuery Query Async.Item (Async.Source Async.Item) Async.Err eff m)
    (Dropdown.Query TestRecord)
    (Typeahead.TypeaheadQuery Query String Void Void eff m)

data SyncTypeaheadSlot
  = SyncTypeaheadStrings
derive instance eqSyncTypeaheadSlot :: Eq SyncTypeaheadSlot
derive instance ordSyncTypeaheadSlot :: Ord SyncTypeaheadSlot

data TypeaheadSlot
  = TypeaheadTodos
  | TypeaheadUsers
derive instance eqTypeaheadSlot :: Eq TypeaheadSlot
derive instance ordTypeaheadSlot :: Ord TypeaheadSlot

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
    render st = container Sidebar.cnNavSections cnDocumentationBlocks

    eval :: Query ~> H.ParentDSL State Query (ChildQuery (Effects eff) m) ChildSlot Void m
    eval (NoOp next) = pure next

    -- No messages necessary to handle, really.
    eval (HandleSyncTypeahead m next) = pure next

    -- Responsible for fetching data based on source and returning it to the component.
    -- Done asynchronously so data can load in the background.
    eval (HandleTypeahead slot m next) = case m of
      Typeahead.RequestData source -> do
        _ <- H.fork do
          res <- H.liftAff $ Async.load source
          H.query' CP.cp1 slot $ H.action $ Typeahead.FulfillRequest $ replaceItems res source
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
    replaceItems (Just d) (Typeahead.ContinuousAsync db srch src _) = Typeahead.ContinuousAsync db srch src d


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

container :: ∀ i p
  . Array (Tuple String (Array (Tuple String String)))
 -> Array (H.HTML i p)
 -> H.HTML i p
container navs blocks =
  HH.body
  [ css "font-sans font-normal text-black leading-normal"
  , HP.class_ (HH.ClassName "bg-grey-lightest")
  ]
  [ HH.div
    [ css "min-h-screen" ]
    [ Sidebar.sidebar navs
    , innerContainer "CitizenNet UI Guide" blocks
    ]
  ]

innerContainer :: ∀ i p
  . String
 -> Array (H.HTML i p)
 -> H.HTML i p
innerContainer title blocks =
  HH.div
  [ css "md:ml-80" ]
  [ HH.div
    [ css "fixed w-full z-20" ]
    [ HH.div
      [ css "pin-t bg-white md:hidden relative border-b border-grey-light h-12 py-8 flex items-center" ]
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

cnDocumentationBlocks :: ∀ eff m. MonadAff (Effects eff) m => Array (H.ParentHTML Query (ChildQuery (Effects eff) m) ChildSlot m)
cnDocumentationBlocks =
  navigationTabBlock
  <> switchBlock
  <> radioBlock
  <> inputBlock
  <> formInputBlock
  <> typeaheadBlockStrings
  <> typeaheadBlockUsers
  <> typeaheadBlockTodos
  <> dropdownBlock
  <> buttonBlock

navigationTabBlock
  :: ∀ eff m
  . MonadAff (Effects eff) m
  => Array (H.ParentHTML Query (ChildQuery (Effects eff) m) ChildSlot m)
navigationTabBlock = documentationBlock
  "Navigation Tabs"
  "Tabs for navigating, eg. between form pages"
  (componentBlock "No configuration set." (NavigationTab.navigationTabs tabConfig))

switchBlock :: ∀ eff m. MonadAff (Effects eff) m => Array (H.ParentHTML Query (ChildQuery (Effects eff) m) ChildSlot m)
switchBlock = documentationBlock
  "switch"
  "some toggle switch shit"
  ( HH.div_
    [ Toggle.toggle []
    ]
  )
radioBlock :: ∀ eff m. MonadAff (Effects eff) m => Array (H.ParentHTML Query (ChildQuery (Effects eff) m) ChildSlot m)
radioBlock = documentationBlock
  "Radio"
  "A radio button"
  ( HH.div_
    [ Radio.radio
      { label: "Apples" }
      [ HP.name "fruit" ]
    , Radio.radio
      { label: "Bananas" }
      [ HP.name "fruit" ]
    , Radio.radio
      { label: "Oranges" }
      [ HP.name "fruit" ]
    ]
  )

inputBlock :: ∀ eff m. MonadAff (Effects eff) m => Array (H.ParentHTML Query (ChildQuery (Effects eff) m) ChildSlot m)
inputBlock = documentationBlock
  "Input"
  "Some input shit"
  ( HH.div_
      [ Input.input [ HP.placeholder "address@gmail.com" ] ]
  )

formInputBlock :: ∀ eff m. MonadAff (Effects eff) m => Array (H.ParentHTML Query (ChildQuery (Effects eff) m) ChildSlot m)
formInputBlock = documentationBlock
  "Input Form Block"
  "Some form input shit"
  ( HH.div_
    [ FormControl.formControl
      { label: "Email"
      , helpText: (Just "You really should fill this shit out")
      }
      (Input.input [ HP.placeholder "address@gmail.com" ])
    ]
  )

buttonBlock :: ∀ eff m. MonadAff (Effects eff) m => Array (H.ParentHTML Query (ChildQuery (Effects eff) m) ChildSlot m)
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
        SyncTypeaheadStrings
        Typeahead.component
        ( Typeahead.defaultMulti' containerData )
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
        TypeaheadTodos
        Typeahead.component
        ( Typeahead.defaultContAsyncMulti' Async.todos )
        (HE.input $ HandleTypeahead TypeaheadTodos)

typeaheadBlockUsers :: ∀ eff m
  . MonadAff (Effects eff) m
 => Array (H.ParentHTML Query (ChildQuery (Effects eff) m) ChildSlot m)
typeaheadBlockUsers = documentationBlock
  "Typeahead"
  "Uses string input to search pre-determined entries."
  ( componentBlock "Set to default sync." slot )
  where
    slot =
      HH.slot'
        CP.cp1
        TypeaheadUsers
        Typeahead.component
        ( Typeahead.defaultAsyncMulti' Async.users )
        (HE.input $ HandleTypeahead TypeaheadUsers)

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
