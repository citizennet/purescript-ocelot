module UIGuide.Utilities.Form where

import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import Data.Variant (Variant)
import Ocelot.Form (FormField, FormInput)


-----
-- Pre-Built Common Field Types

type Attrs more = ( label :: String, helpText :: String | more )
type Err e = Array (Variant e)

type Email f = f (Attrs ()) EmailError String
type EmailError = Err
  ( emptyField :: String
  , badEmail :: String )

type PasswordEq f = f (Attrs ()) PasswordErrorEq String
type PasswordErrorEq = Err
  ( emptyField :: String
  , notEqual :: Tuple (Maybe String) (Maybe String)
  )

type Password f = f (Attrs ()) PasswordError String
type PasswordError = Err
  ( emptyField :: String
  )

-----
-- Helpers

-- The form types we need: the raw form, the initial form, and the overall signup form.
type FormField' (a :: # Type) b c = FormField c
type FormInput' vl vd (a :: # Type) b c
  = FormInput a (c -> vl) (Boolean -> vd) b c
type FormMaybe' (a :: # Type) b c = Maybe c

