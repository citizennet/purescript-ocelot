module UIGuide.Components.Validation where

import Prelude

import Data.Either (Either(..))
import Data.Symbol (SProxy(..))
import Data.Variant (inj)
import Ocelot.Core.Form (formFromField, runForm)
import Ocelot.Core.Validation (validateNonEmptyStr, validateStrIsEmail)
import Polyform.Validation (hoistFnV)

----------
-- Form

--  -- These fields are available in our form
--  _password = SProxy :: SProxy "password"
--  _email = SProxy :: SProxy "email"
--
--  signupForm = { password: _, email: _ }
--    <$> formFromField _password (hoistFnV validateNonEmptyStr)
--    <*> formFromField _email
--        ( hoistFnV validateNonEmptyStr
--          >>>
--          hoistFnV (validateStrIsEmail "Not an email")
--        )
--
--  runSignupForm =
--    runForm
--      signupForm
--      { password:
--        { validated: Right ""
--        , setValue: inj _password
--        , setValidate: inj _password
--        }
--      , email:
--        { validated: Right ""
--        , setValue: inj _email
--        , setValidate: inj _password
--        }
--      }
--
--  badEmail =
--    { password:
--      { value: "longenough89"
--      , shouldValidate: true
--      }
--    , email:
--      { value: "notlongenough89"
--      , shouldValidate: false
--      }
--    }
--
--  badPass =
--    { password:
--      { value: ""
--      , shouldValidate: true
--      }
--    , email:
--      { value: "thomas@thomas.thomas"
--      , shouldValidate: false
--      }
--    }
--
--  good =
--    { password:
--      { value: "longenough90"
--      , shouldValidate: true
--      }
--    , email:
--      { value: "thomas@thomas.thomas"
--      , shouldValidate: false
--      }
--    }
