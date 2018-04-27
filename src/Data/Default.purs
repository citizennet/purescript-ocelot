module Ocelot.Data.Default where

import Prelude

import Data.List (List)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Tuple (Tuple(..))

class Default v where
  def :: v

instance defaultUnit :: Default Unit where
  def = mempty

instance defaultInt :: Default Int where
  def = 0

instance defaultNumber :: Default Number where
  def = 0.0

instance defaultString :: Default String where
  def = mempty

instance defaultOrdering :: Default Ordering where
  def = mempty

instance defaultMaybe :: Default (Maybe a) where
  def = Nothing

instance defaultArray :: Default (Array a) where
  def = mempty

instance defaultList :: Default (List a) where
  def = mempty

instance defaultMap :: Ord k => Default (Map k v) where
  def = mempty

instance defaultFn :: Default b => Default (a -> b) where
  def = const def

instance defaultTuple :: (Default a, Default b) => Default (Tuple a b) where
  def = Tuple def def
