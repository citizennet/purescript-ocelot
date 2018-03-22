module Ocelot.Data.Sorting where

import Prelude


-----
-- Data types and helpers for handling sorting operations

data Sorting = Sorting SortDirection SortKey

data SortDirection
  = Ascending
  | Descending

type SortKey = String

instance showSortDirection :: Show SortDirection where
  show Ascending = "asc"
  show Descending = "desc"

instance showSorting :: Show Sorting where
  show (Sorting direction key) = "&order_by=" <> key <> "&order_direction=" <> show direction

invert :: SortDirection -> SortDirection
invert Ascending = Descending
invert Descending = Ascending

updateSorting :: Sorting -> SortKey -> Sorting
updateSorting (Sorting direction key) selectedKey | key == selectedKey =
  Sorting (invert direction) key
updateSorting (Sorting direction _) selectedKey =
  Sorting direction selectedKey


