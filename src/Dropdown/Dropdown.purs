module CN.UI.Dropdown where

import Prelude

import Select.Dispatch
import Select.Effects (FX)
import Select.Primitive.Container as C

{-

Type Configuration

-}

data DropdownItem
  = DropdownItem SelectedStatus SelectableStatus String

data SelectedStatus
  = Selected
  | NotSelected

data SelectableStatus
  = Selectable
  | NotSelectable

type State =
  { items :: Array DropdownItem
  , selections :: Array DropdownItem }


data Query e a
  = HandleContainer (C.Message String (Query e) e) a


{-

Component Definition

-}


{-

Render Helpers

-}
