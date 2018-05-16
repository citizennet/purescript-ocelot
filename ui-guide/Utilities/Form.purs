module UIGuide.Utilities.Form where

import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import Data.Variant (Variant)
import Ocelot.Form (FormInput)


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
type FormInput' vl vd e a b = FormInput (a -> vl) (Boolean -> vd) e a b
type FormMaybe' a b c = Maybe c
