module UIGuide.Components.Validation where

import Prelude

import Data.Either (Either)
import Data.Monoid (class Monoid)
import Data.Symbol (SProxy(..))
import Data.Validation.Semigroup (V)
import Data.Variant (Variant)
import Ocelot.Core.Form (Form, formFromField, hoistFn, runForm)
import Ocelot.Core.Validation (validateNonEmptyStr, validateStrIsEmail)

----------
-- Form

-- These fields are available in our form
_password = SProxy :: SProxy "password"
_email = SProxy :: SProxy "email"

signupForm :: ∀ t12 t13 t14 t15 m t26 t29 t30 t31 t32 t45 t46 t47
  . Monad m
 => Monoid t12
 => Monoid t29
 => Form m
     -- Our form (Endo (Record form)) where transformations will
     -- occur -- Either type is wrong.
     ( password ::
       { validated
         :: Either t12 (V (Array (Variant (emptyField :: String | t26))) String)
       , setValue :: t14
       , setValidate :: t13
       | t15
       }
     , email ::
       { validated
         :: Either t29 (V (Array (Variant (badEmail :: String | t45))) String)
       , setValue :: t31
       , setValidate :: t30
       | t32
       }
     | t47
     )

     -- Our input (Record input), which validation will run on. This is right.
     ( password ::
       { value :: String
       , shouldValidate :: Boolean
       }
     , email ::
       { value :: String
       , shouldValidate :: Boolean
       }
     | t46
     )

     -- Our output. This is wrong. It should be just { string, string }.
     { password :: V (Array (Variant (emptyField :: String | t26))) String
     , email :: V (Array (Variant (badEmail :: String | t45))) String
     }
signupForm = { password: _, email: _ }
  <$> formFromField _password (hoistFn validateNonEmptyStr)
  <*> formFromField _email (hoistFn $ validateStrIsEmail "Not an email")

runSignupForm :: ∀ m t53 t55 t56 t57 t58 t59 t60 t62 t63 t64 t65
  . Monad m
 => Monoid t65
 => Monoid t59
 => m
    ( Either
       -- Input form with errors -- validated should be Either e a, not
       -- Either t0 (V e a) -- but not sure how to fix this.
       { password ::
         { validated
           :: Either t65 (V (Array (Variant (emptyField :: String | t60))) String)
         , setValue :: t63
         , setValidate :: t64
         | t62
         }
       , email ::
         { validated
           :: Either t59 (V (Array (Variant (badEmail :: String | t55))) String)
         , setValue :: t57
         , setValidate :: t58
         | t56
         }
       | t53
       }

       -- Output form successfully parsed -- should be strings, not V
       { password :: V (Array (Variant (emptyField :: String | t60))) String
       , email :: V (Array (Variant (badEmail :: String | t55))) String
       }
    )
runSignupForm =
  runForm
    signupForm
    { password:
      { value: "longenough89"
      , shouldValidate: true
      }
    , email:
      { value: "notlongenough89"
      , shouldValidate: false
      }
    }
