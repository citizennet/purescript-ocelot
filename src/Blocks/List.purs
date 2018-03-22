module Ocelot.Block.List where

import Prelude

import DOM.Event.MouseEvent (MouseEvent)
import Data.Maybe (Maybe)
import Data.Monoid (mempty)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Ocelot.Data.Sorting (Sorting(..), SortDirection(..), SortKey)


-----
-- Some basic common structure for building list components

table
  :: ∀ p i
   . Array (HH.HTML p i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
table columns rows =
  HH.div
    [ HP.class_ (HH.ClassName "cn-list container-fluid") ]
    [ HH.table
      [ HP.class_ (HH.ClassName "table card-flex") ]
      [ HH.thead_
        [ HH.tr_ columns ]
      , HH.tbody_ rows
      ]
    ]

header
  :: ∀ p i
   . Sorting
  -> (SortKey -> MouseEvent -> Maybe i)
  -> SortKey
  -> String
  -> HH.HTML p i
header sorting action sortKey label =
  HH.th
    [ HE.onClick (action sortKey)
    , HP.class_ (HH.ClassName "cn-pointable")
    ]
    [ HH.text label
    , HH.i
      [ HP.class_ (sortIcon sorting) ]
      []
    ]
  where
    sortIcon :: Sorting -> HH.ClassName
    sortIcon (Sorting _ key) | sortKey /= key = HH.ClassName mempty
    sortIcon (Sorting Ascending _) = HH.ClassName "active fa fa-arrow-up margin-left-5"
    sortIcon (Sorting Descending _) = HH.ClassName "active fa fa-arrow-down margin-left-5"

