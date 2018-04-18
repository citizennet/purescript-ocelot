module UIGuide.Components.Validation where

import Ocelot.Core.Validation
import Prelude

import Data.Symbol (SProxy(..))
import Polyform.Validation as Polyform
import UIGuide.Utilities.Form (collapseIfEqual, mkForm)

----------
-- Form

-- These fields are available in our form
_password1 = SProxy :: SProxy "password1"
_password2 = SProxy :: SProxy "password2"
_email = SProxy :: SProxy "email"

-- Make fields with preset validation
password sym =
  mkForm sym (validateNonEmptyStr)

email sym =
  mkForm sym (validateNonEmptyStr *> validateStrIsEmail "Not an email")

-- Make a two-password form  that will parse to a single password
passwordForm = ( { p1: _, p2: _ } <$> password _password1 <*> password _password2 )
 >>> Polyform.hoistFnV \{ p1, p2 } -> collapseIfEqual p1 p2 _password1 _password2

signupForm = { password: _, email: _ } <$> passwordForm <*> email _email
