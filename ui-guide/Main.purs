module Main where

import Prelude

import Effect (Effect)
import Halogen.Aff as HA
import UIGuide.App (runStorybook)
import UIGuide.App.Routes (routes, groups)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runStorybook routes groups body
