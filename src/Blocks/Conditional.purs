module Ocelot.Block.Conditional where

import DOM.HTML.Indexed (HTMLdiv)
import Halogen.HTML as HH

conditional
  :: ∀ p i
   . Boolean
  -> Array (HH.IProp HTMLdiv i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
conditional cond props html =
  HH.div props if cond then html else []

conditional_
  :: ∀ p i
   . Boolean
  -> Array (HH.HTML p i)
  -> HH.HTML p i
conditional_ cond =
  conditional cond []

alt
  :: ∀ p i
   . Boolean
  -> Array (HH.IProp HTMLdiv i)
  -> Array (HH.HTML p i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
alt cond props html1 html2 =
  HH.div props if cond then html1 else html2

alt_
  :: ∀ p i
   . Boolean
  -> Array (HH.HTML p i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
alt_ cond =
  alt cond []
