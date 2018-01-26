module Main where

import Prelude
import UIGuide.Components.TextFields as TextFields

import Control.Monad.Eff (Eff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Select.Effects (Effects)

main :: ∀ e. Eff (HA.HalogenEffects (Effects (console :: CONSOLE | e))) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI TextFields.component unit body
