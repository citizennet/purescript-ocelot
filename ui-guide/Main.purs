module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Halogen.Aff as HA

import UIGuide.App (Stories, runStorybook, proxy)
import UIGuide.App.Routes (routes)


main :: ∀ eff. Eff (HA.HalogenEffects _) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runStorybook routes body
