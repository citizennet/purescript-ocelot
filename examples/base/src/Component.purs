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
    render :: State -> H.ParentHTML Query (ChildQuery e) ChildSlot (FX e)
    render st =
      HH.div_
      [ HH.slot'
          CP.cp1
          unit
          Dropdown.component
          { items: containerData }
          absurd
      , HH.slot'
          CP.cp2
          unit
          Typeahead.component
          { items: containerData }
          absurd
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
