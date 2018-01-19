module Base.Component where

import Prelude

import CN.UI.Dropdown as Dropdown
import CN.UI.Typeahead as Typeahead
import Data.Either.Nested (Either2)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Component.ChildPath as CP
import Select.Effects (FX)

type State
  = Unit

data Query a
  = NoOp a

----------
-- Child paths

type ChildQuery e = Coproduct2 (Dropdown.Query e) (Typeahead.Query e)
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
    render st =
      HH.section
      [ HP.class_ $ HH.ClassName "mw6 mw7-ns center pa2 ph4-ns black-80 sans-serif" ]
      [ title
      , introduction
      , mountWith
          "A plain dropdown component that only allows a single selection."
          (HH.slot' CP.cp1 unit Dropdown.component { items: containerData } absurd)
          ""
      , mountWith
          "A typeahead that allows for multiple selections and renders custom items."
          (HH.slot' CP.cp2 unit Typeahead.component { items: containerData } absurd)
          ""
      ]

    eval :: Query ~> H.ParentDSL State Query _ _ _ (FX e)
    eval (NoOp a) = pure a


----------
-- Sample data

containerData :: Array Dropdown.DropdownItem
containerData =
  [ "item one"
  , "item two"
  , "who knows" ]


----------
-- CSS helpers

cssTitle = HP.class_ $ HH.ClassName "f1 lh-title"
cssIntroText = HP.class_ $ HH.ClassName "f4 lh-copy"
cssExampleBlock = HP.class_ $ HH.ClassName "pa4 bg-near-white"
cssPullQuote = HH.ClassName "pl4 mb4 bl bw2 b--blue f5 lh-copy"
cssPostScript = HH.ClassName "f5 mt4 lh-copy black-60 i"

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
mountWith before slot after =
  HH.div
  [ cssExampleBlock ]
  [ HH.p [ HP.class_ cssPullQuote ] [ HH.text before ]
  , slot
  , HH.p [ HP.class_ cssPostScript ] [ HH.text after ]
  ]

