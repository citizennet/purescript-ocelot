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
      [ HP.class_ $ HH.ClassName "mw5 mw7-ns center pa2 ph4-ns" ]
      [ mount (HH.slot' CP.cp1 unit Dropdown.component { items: containerData } absurd)
      , mount (HH.slot' CP.cp2 unit Typeahead.component { items: containerData } absurd)
      ]
      where
        introduction :: HTML e
        introduction =
          HH.h1
          [ HP.class_ $ HH.ClassName "f1 lh-title" ]
          [ HH.text "CitizenNet Select Components" ]

        mountWith :: _ -> _ -> _ -> HTML e
        mountWith before slot after =
          HH.div
          [ HP.class_ $ HH.ClassName "pa3 ph0-1 bb b--black-10" ]
          ( before <> slot <> after )

        mount :: _ -> HTML e
        mount slot = mountWith [] [slot] []

        -- intro title body =



    eval :: Query ~> H.ParentDSL State Query _ _ _ (FX e)
    eval (NoOp a) = pure a


----------
-- Sample data

containerData :: Array Dropdown.DropdownItem
containerData =
  [ "item one"
  , "item two"
  , "who knows" ]
