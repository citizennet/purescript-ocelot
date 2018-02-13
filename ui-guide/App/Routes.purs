module UIGuide.App.Routes
  ( routes )
where

import Prelude

import Data.StrMap as SM
import UIGuide.App (proxy)
import Data.Tuple (Tuple(..))

import UIGuide.Components.Button as Button
import UIGuide.Components.FormControl as FormControl
import UIGuide.Components.TextFields as TextFields
import UIGuide.Components.Validation as Validation

routes = SM.fromFoldable
  [ Tuple "Text Fields" $ proxy $ TextFields.component
  , Tuple "Buttons" $ proxy $ Button.component
  , Tuple "Form Controls" $ proxy $ FormControl.component
  , Tuple "Validation" $ proxy $ Validation.component
  ]

