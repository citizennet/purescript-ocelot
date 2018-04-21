module Main where

import Prelude

import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Timer (TIMER)
import Debug.Trace (traceAnyA)
import Halogen.Aff as HA
import Network.HTTP.Affjax (AJAX)
import UIGuide.App (runStorybook)
import UIGuide.App.Routes (routes, groups)
import UIGuide.Components.Validation (runSignupForm)

type Effects =
  ( ajax :: AJAX
  , console :: CONSOLE
  , timer :: TIMER
  )

main :: Eff (HA.HalogenEffects Effects) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  r <- runSignupForm
  traceAnyA r
  runStorybook routes groups body
