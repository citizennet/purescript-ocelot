module Ocelot.HTML.Properties
  ( testId
  , css
  , appendIProps
  , (<&>)
  ) where

import Prelude

import Data.Array (elem, foldr, nubBy, snoc)
import Data.Bifunctor (lmap, rmap)
import Data.String (Pattern(..), drop, null, split)
import Data.String.Utils (startsWith)
import Data.Tuple (Tuple(..))
import Halogen.HTML as HH
import Halogen.HTML.Core (Prop(..))
import Halogen.HTML.Properties as HP
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
    classNames =
      pure
      <<< HP.classes
        $ HH.ClassName
      <$> nubBy
          (\c c' -> classify c == classify c')
          (classes' <> classes)

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

classify
  :: String
  -> String
classify str
  | startsWith "p" str && not null (classifySide $ drop 1 str)
    = "padding" <-> classifySide (drop 1 str)
  | startsWith "m" str && not null (classifySide $ drop 1 str)
    = "margin" <-> classifySide (drop 1 str)
  | startsWith "-m" str && not null (classifySide $ drop 2 str)
    = "margin" <-> classifySide (drop 2 str)
  | startsWith "min-" str = "min" <-> classify (drop 4 str)
  | startsWith "max-" str = "max" <-> classify (drop 4 str)
  | startsWith "w-" str = "width"
  | startsWith "h-" str = "height"
  | startsWith "overflow-" str && (classifyOverflow $ drop 9 str) /= drop 9 str
    = "overflow" <-> (classifyOverflow $ drop 9 str)
  | otherwise = str

classifySide
  :: String
  -> String
classifySide str
  | startsWith "t-" str = "top"
  | startsWith "r-" str = "right"
  | startsWith "b-" str = "bottom"
  | startsWith "l-" str = "left"
  | startsWith "x-" str = "horizontal"
  | startsWith "y-" str = "vertical"
  | startsWith "-" str = "all"
  | otherwise = ""

classifyOverflow
  :: String
  -> String
classifyOverflow str
  | startsWith "x-" str = "horizontal" <-> (classifyOverflow $ drop 2 str)
  | startsWith "y-" str = "vertical" <-> (classifyOverflow $ drop 2 str)
  | elem str ["auto", "hidden", "visible", "scroll"] = ""
  | otherwise = str

append'
  :: String
  -> String
  -> String
append' x "" = x
append' x y  = x <> "-" <> y

infixr 5 append' as <->
