module Base.Component where

import Prelude

import CN.UI.Dropdown as Dropdown
import CN.UI.Multiselect (MultiselectMessage)
import CN.UI.Multiselect as Multiselect
import CN.UI.Typeahead as Typeahead
import Data.Either.Nested (Either2, Either3)
import Data.Functor.Coproduct.Nested (Coproduct2, Coproduct3)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.Component.ChildPath (cp1)
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
  | HandleDropdown (Dropdown.DropdownMessage DropdownItem) a
  | HandleMultiselect (Multiselect.MultiselectMessage DropdownItem) a


----------
-- Child paths

type ChildQuery e = Coproduct3
  (Dropdown.Query DropdownItem e)
  (Multiselect.Query DropdownItem e)
  (Typeahead.Query e)
type ChildSlot = Either3 Unit Unit Unit


----------
-- Item specializations

type DropdownItem = String

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
    render st =
      HH.section
      [ HP.class_ $ HH.ClassName "mw6 mw7-ns center pa2 ph4-ns black-80 sans-serif" ]
      [ title
      , introduction
      , mountWith
          "Select Field"
          "Selects a single item from a list."
          ( HH.slot'
              CP.cp1
              unit
              Dropdown.component
              { items: containerData, itemHTML: (\i -> [ HH.text i ]) }
              (HE.input HandleDropdown) )
      , mountWith
          "Multiselect Field"
          "Selects multiple items from a list."
          ( HH.slot'
              CP.cp2
              unit
              Multiselect.component
              { items: containerData, itemHTML: (\i -> [ HH.text i ]), selection: ["item one"] }
              (HE.input HandleMultiselect) )
      , mountWith
          "Typeahead"
          "Captures string input and produces a menu."
          ( HH.slot' CP.cp3 unit Typeahead.component { items: containerData } absurd )
      ]

    eval :: Query ~> H.ParentDSL State Query _ _ _ (FX e)
    eval (NoOp next) = pure next
    eval (HandleDropdown _ next) = pure next
    eval (HandleMultiselect _ next) = pure next


----------
-- Sample data

containerData :: Array DropdownItem
containerData =
  [ "item one"
  , "item two"
  , "who knows" ]


----------
-- CSS helpers

cssTitle = HP.class_ $ HH.ClassName "f1 lh-title"
cssExampleTitle = HP.class_ $ HH.ClassName "f2 lh-title"
cssExampleDescription = HP.class_ $ HH.ClassName "f4 lh-copy black-70"
cssIntroText = HP.class_ $ HH.ClassName "f4 lh-copy"
cssExampleBlock = HP.class_ $ HH.ClassName "pa4 bg-near-white"
cssPullQuote = HP.class_ $ HH.ClassName "pl4 mb4 bl bw2 b--blue f5 lh-copy"
cssPostScript = HP.class_ $ HH.ClassName "f5 mt4 lh-copy black-60 i"

title :: ∀ e. HTML e
title =
  HH.h1
  [ cssTitle ]
  [ HH.text "CitizenNet Select Components" ]

introduction :: ∀ e. HTML e
introduction =
  HH.p
  [ cssIntroText ]
  [ HH.text "A demonstration of various pre-built selection components ready to be dropped in to Wildcat screens. This can be used to verify that components behave and look as expected & required." ]

-- mountWith :: ∀ e. String -> _ -> String -> HTML e
mountWith title body slot =
  HH.div
  [ ]
  [ HH.h2 [ cssExampleTitle ] [ HH.text title ]
  , HH.p  [ cssExampleDescription ] [ HH.text body ]
  , HH.div [ cssExampleBlock ] [ slot ]
  ]

