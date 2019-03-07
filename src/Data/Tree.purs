module Ocelot.Data.Tree where

import Prelude

import Data.Argonaut (class DecodeJson, decodeJson, (.?=), (.??))
import Data.Function (on)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (SProxy(..))

newtype Node a = Node
  { value :: a
  , children :: Array (Node a)
  , expanded :: Boolean
  , selected :: Boolean
  }

derive instance newtypeNode :: Newtype (Node a) _

instance eqNode :: Eq a => Eq (Node a) where
  eq = eq `on` (_.value <<< unwrap)

instance decodeJsonNode :: DecodeJson a => DecodeJson (Node a) where
  decodeJson json = do
    x <- decodeJson json
    value <- decodeJson json
    children <- x .?? "children" .?= []
    pure $ Node { value, children, expanded: false, selected: false }

type ItemPath a = Array a

type IndexPath = Array Int

_selected :: ∀ a. Lens' (Node a) Boolean
_selected = _Newtype <<< prop (SProxy :: SProxy "selected")

_expanded :: ∀ a. Lens' (Node a) Boolean
_expanded = _Newtype <<< prop (SProxy :: SProxy "expanded")

_children :: ∀ a. Lens' (Node a) (Array (Node a))
_children = _Newtype <<< prop (SProxy :: SProxy "children")
