module Test.Main where

import Prelude

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Maybe (Maybe)
import Debug.Trace (traceAnyA)
import Ocelot.Data.Record (makeDefaultFormFields, makeDefaultFormInputs)
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

type Fields =
  ( email :: String
  , password :: Maybe String
  )

main :: ∀ eff. Eff (Effects eff) Unit
main = runTest do
  suite "Validation" do
    --  traceAnyA $ makeDefaultFormFields (RProxy :: RProxy Fields)
    traceAnyA $ makeDefaultFormInputs (RProxy :: RProxy Fields)
    pure unit
