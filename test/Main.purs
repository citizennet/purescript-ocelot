module Test.Main where

import Prelude

import Effect (Effect)
import Data.Maybe (Maybe)
import Ocelot.Data.Record (makeDefaultFormInputs)
import Debug.Trace (traceM)
import Test.Unit (suite)
import Test.Unit.Main (runTest)
import Type.Prelude (RProxy(..))

type Fields =
  ( email :: String
  , password :: Maybe String
  )

main :: Effect Unit
main = runTest do
  suite "Validation" do
    traceM $ makeDefaultFormInputs (RProxy :: RProxy Fields)
    pure unit
