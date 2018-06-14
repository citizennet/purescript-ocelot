module Main where

import Prelude

import Effect.Aff.Console (CONSOLE)
import Effect (Effect)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Timer (TIMER)
import Halogen.Aff as HA
import Network.HTTP.Affjax (AJAX)
import UIGuide.App (runStorybook)
import UIGuide.App.Routes (routes, groups)

type Effects =
  ( ajax :: AJAX
  , console :: CONSOLE
  , timer :: TIMER
  , random :: RANDOM
  )

main :: Eff (HA.HalogenEffects Effects) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runStorybook routes groups body
