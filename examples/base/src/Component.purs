module Base.Component where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Data.Maybe (Maybe(..))
import CN.UI.Dropdown as Dropdown
import Select.Effects (FX)

type State
  = Unit

data Query a
  = NoOp a

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
    render :: State -> H.ParentHTML Query _ _ (FX e)
    render st =
      HH.div_
      [ HH.slot unit Dropdown.component { items: dropdownData } (HE.input_ NoOp) ]

    eval :: Query ~> H.ParentDSL State Query _ _ _ (FX e)
    eval (NoOp a) = pure a


----------
--

dropdownData :: Array Dropdown.DropdownItem
dropdownData =
  [ "item one"
  , "item two" ]
