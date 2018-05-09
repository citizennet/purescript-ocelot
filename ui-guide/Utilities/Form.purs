module UIGuide.Utilities.Form where

import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import Data.Variant (Variant)

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
