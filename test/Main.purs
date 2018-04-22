module Test.Main where

import Prelude

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Variant (inj)
import Debug.Trace (traceAnyA)
import Ocelot.Core.Form (formFromField, runForm)
import Ocelot.Core.Validation (validateNonEmptyStr, validateStrIsEmail)
import Polyform.Validation (V(..), hoistFnV)
import Test.Unit (suite, test)
import Test.Unit.Assert (assert, assertFalse)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

type Effects eff =
  ( console :: CONSOLE
  , testOutput :: TESTOUTPUT
  , avar :: AVAR
  | eff
  )

main :: ∀ eff. Eff (Effects eff) Unit
main = runTest do
  suite "Validation" do
    let emailValid = case _ of
          Invalid f -> case f.email.validated of
            Nothing -> false
            Just (Left _) -> false
            (Just (Right _)) -> true
          Valid _ _ -> true

        passValid = case _ of
          Invalid f -> case f.password.validated of
            Nothing -> false
            Just (Left _) -> false
            (Just (Right _)) -> true
          Valid _ _ -> true

    test "Partially validate bad password" do
      res <- runForm testForm initialForm badPass
      traceAnyA res
      assertFalse "Email should work, but password should fail."
        $ passValid res && emailValid res

    test "Partially validate bad email" do
      res <- runForm testForm initialForm badEmail
      traceAnyA res
      assertFalse "Password should work, but email should fail."
        $ passValid res && emailValid res

    test "Validate correct input" do
      res <- runForm testForm initialForm good
      traceAnyA res
      assert "Validation should pass"
        $ passValid res && emailValid res

    test "Validate skipped input" do
      res <- runForm testForm initialForm skip
      traceAnyA res
      assert "Validation should pass, but not parse"
        $ passValid res && emailValid res


----------
-- Helpers for testing

_password = SProxy :: SProxy "password"
_email = SProxy :: SProxy "email"

testForm = { password: _, email: _ }
  <$> formFromField _password (hoistFnV validateNonEmptyStr)
  <*> formFromField _email
      ( hoistFnV validateNonEmptyStr
        >>>
        hoistFnV (validateStrIsEmail "Not an email")
      )

initialForm =
  { password:
    { validated: Nothing
    , setValue: inj _password
    , setValidate: inj _password
    }
  , email:
    { validated: Nothing
    , setValue: inj _email
    , setValidate: inj _password
    }
  }

badEmail =
  { password:
    { value: "longenough89"
    , shouldValidate: true
    }
  , email:
    { value: "notlongenough89"
    , shouldValidate: true
    }
  }

badPass =
  { password:
    { value: ""
    , shouldValidate: true
    }
  , email:
    { value: "thomas@thomas.thomas"
    , shouldValidate: true
    }
  }

good =
  { password:
    { value: "longenough90"
    , shouldValidate: true
    }
  , email:
    { value: "thomas@thomas.thomas"
    , shouldValidate: true
    }
  }

skip =
  { password:
    { value: "longenough90"
    , shouldValidate: false
    }
  , email:
    { value: "thomas@thomas.thomas"
    , shouldValidate: true
    }
  }

