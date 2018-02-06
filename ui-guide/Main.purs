module Main where

import Prelude
import UIGuide.Components.TextFields as TextFields

import Control.Monad.Eff (Eff)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Eff _ Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI TextFields.component unit body
