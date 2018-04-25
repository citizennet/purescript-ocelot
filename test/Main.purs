module Test.Main where

import Prelude

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Either (Either(..))
import Data.Lens (view)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Variant (inj)
import Debug.Trace (traceAnyA)
import Ocelot.Form (_validated, formFromField, runForm)
import Ocelot.Core.Validation (collapseIfEqual, validateNonEmptyStr, validateStrIsEmail)
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
    let isValid field = case _ of
          Invalid f -> case view field f of
            Nothing -> false
            Just (Left _) -> false
            (Just (Right _)) -> true
          Valid _ _ -> true

        emailValid = isValid (prop _email <<< _validated)
        pass1Valid = isValid (prop _password1 <<< _validated)
        pass2Valid = isValid (prop _password2 <<< _validated)

    pure unit
    test "Partially validate bad password" do
      res <- runForm testForm initialForm badPass
      traceAnyA res
      assertFalse "Email should work, but password should fail."
        $ emailValid res && pass1Valid res && pass2Valid res

    test "Partially validate bad email" do
      res <- runForm testForm initialForm badEmail
      traceAnyA res
      assertFalse "Password should work, but email should fail."
        $ emailValid res && pass1Valid res && pass2Valid res
    --
    test "Validate correct input" do
      res <- runForm testForm initialForm good
      traceAnyA res
      assert "Validation should pass"
        $ emailValid res && pass1Valid res && pass2Valid res
    --
    test "Validate skipped input" do
      res <- runForm testForm initialForm skip
      traceAnyA res
      assert "Validation should pass, but not parse"
        $ emailValid res && pass1Valid res && pass2Valid res


----------
-- Helpers for testing

_password = SProxy :: SProxy "password"
_password1 = SProxy :: SProxy "password1"
_password2 = SProxy :: SProxy "password2"
_email = SProxy :: SProxy "email"

password1Form = formFromField _password1 $ hoistFnV validateNonEmptyStr
password2Form = formFromField _password2 $ hoistFnV validateNonEmptyStr

passwordForm = (
  { password1: _, password2: _ }
  <$> password1Form
  <*> password2Form )
  >>> validatePasswords
  where
    validatePasswords = hoistFnV \{ password1, password2 } ->
      collapseIfEqual password1 password2 _password1 _password2

emailForm = formFromField _email $
  hoistFnV validateNonEmptyStr
  >>> hoistFnV (validateStrIsEmail "Not a valid email!")

testForm = { password: _, email: _ }
  <$> passwordForm
  <*> emailForm

initialForm =
  { password1:
    { validated: Nothing
    , setValue: inj _password1
    , setValidate: inj _password1
    }
  , password2:
    { validated: Nothing
    , setValue: inj _password2
    , setValidate: inj _password2
    }
  , email:
    { validated: Nothing
    , setValue: inj _email
    , setValidate: inj _password
    }
  }

badEmail =
  { password1:
    { value: "longenough89"
    , shouldValidate: true
    }
  , password2:
    { value: "longenough89"
    , shouldValidate: true
    }
  , email:
    { value: "notlongenough89"
    , shouldValidate: true
    }
  }

badPass =
  { password1:
    { value: ""
    , shouldValidate: true
    }
  , password2:
    { value: ""
    , shouldValidate: true
    }
  , email:
    { value: "thomas@thomas.thomas"
    , shouldValidate: true
    }
  }

good =
  { password1:
    { value: "longenough90"
    , shouldValidate: true
    }
  , password2:
    { value: "longenough90"
    , shouldValidate: true
    }
  , email:
    { value: "thomas@thomas.thomas"
    , shouldValidate: true
    }
  }

skip =
  { password1:
    { value: "longenough90"
    , shouldValidate: false
    }
  , password2:
    { value: "longenough90"
    , shouldValidate: false
    }
  , email:
    { value: "thomas@thomas.thomas"
    , shouldValidate: true
    }
  }

