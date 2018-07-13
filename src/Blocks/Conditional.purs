module Ocelot.Block.Conditional where

import DOM.HTML.Indexed (HTMLspan)
import Halogen.HTML as HH

conditional
  :: ∀ p i
   . Boolean
  -> Array (HH.IProp HTMLspan i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
conditional cond props html =
  HH.span props if cond then html else []

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
  -> Array (HH.IProp HTMLspan i)
  -> Array (HH.HTML p i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
alt cond props html1 html2 =
  HH.span props if cond then html1 else html2

alt_
  :: ∀ p i
   . Boolean
  -> Array (HH.HTML p i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
alt_ cond =
  alt cond []
