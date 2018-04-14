module Ocelot.Core.Utils
  ( css
  , appendIProps
  , (<&>)
  , testId
  ) where

import Prelude

import Data.Array (nub, snoc)
import Data.Bifunctor (rmap, lmap)
import Data.Foldable (foldr)
import Data.String (Pattern(..), split)
import Data.Tuple (Tuple(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.VDom.DOM.Prop (Prop(..))
import Unsafe.Coerce (unsafeCoerce)

type IProp r i = HH.IProp ("class" :: String | r) i

testId
  :: ∀ r i
   . String
  -> IProp r i
testId = HP.attr (HH.AttrName "data-testid")

css
  :: ∀ r i
   . String
  -> IProp r i
css = HP.class_ <<< HH.ClassName

appendIProps
  :: ∀ r i
   . Array (IProp r i)
  -> Array (IProp r i)
  -> Array (IProp r i)
appendIProps ip ip' =
  iprops <> iprops' <> classNames
  where
    (Tuple classes iprops) = extract ip
    (Tuple classes' iprops') = extract ip'
    classNames = pure <<< HP.classes $ HH.ClassName <$> nub (classes <> classes')

infixr 5 appendIProps as <&>

extract
  :: ∀ r i
   . Array (IProp r i)
  -> Tuple (Array String) (Array (IProp r i))
extract =
  foldr f (Tuple [] [])
  where
    f (HP.IProp (Property "className" className)) =
      lmap (\_ -> (split (Pattern " ") <<< unsafeCoerce) className)
    f iprop =  rmap $ (flip snoc) iprop
