module Test.Main where

import Prelude

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Maybe (Maybe)
import Debug.Trace (traceAnyA)
import Ocelot.Form (makeRawForm)
import Ocelot.Data.Default (class Default)
import Test.Unit (suite)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)
import Type.Prelude (RProxy(..))

type Effects eff =
  ( console :: CONSOLE
  , testOutput :: TESTOUTPUT
  , avar :: AVAR
  | eff
  )

data Summer = Winter | Fall

instance defaultSummer :: Default Summer where
  def = Winter

type FormFields =
  ( email :: String
  , password :: Maybe Summer
  )

main :: ∀ eff. Eff (Effects eff) Unit
main = runTest do
  suite "Validation" do
    traceAnyA $ makeRawForm (RProxy :: RProxy FormFields)
    pure unit
