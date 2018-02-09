module Main where

import Prelude
import UIGuide.Components.TextFields as TextFields
import UIGuide.Router (Stories, runStorybook, proxy)

import Control.Monad.Eff (Eff)
import Halogen.Aff as HA

import Data.StrMap as SM
import Data.Tuple (Tuple(..))

stories = SM.fromFoldable
  [ Tuple "textfields" $ proxy $ TextFields.component
  ]

main :: ∀ eff. Eff (HA.HalogenEffects (TextFields.Effects eff)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runStorybook stories body
