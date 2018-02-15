module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Halogen.Aff as HA

import UIGuide.App (runStorybook)
import UIGuide.App.Routes (routes, groups)

main :: Eff (HA.HalogenEffects _) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runStorybook routes groups body
