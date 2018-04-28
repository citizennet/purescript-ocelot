module UIGuide.Utilities.Form where

import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import Data.Variant (Variant)
import Ocelot.Form (FormField, FormInput)


-----
-- Pre-Built Common Field Types

type Err e = Array (Variant e)

type EmailError = Err
  ( emptyField :: String
  , badEmail :: String )

type PasswordErrorEq = Err
  ( emptyField :: String
  , notEqual :: Tuple (Maybe String) (Maybe String)
  )

type PasswordError = Err
  ( emptyField :: String
  )

-----
-- Helpers

-- The form types we need: the raw form, the initial form, and the overall signup form.
type FormField' a b = FormField b
type FormInput' vl vd a b
  = FormInput (b -> vl) (Boolean -> vd) a b
type FormMaybe' a b = Maybe b

