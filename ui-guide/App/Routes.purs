module UIGuide.App.Routes
  ( routes )
where

import Data.StrMap as SM
import UIGuide.App (proxy)
import Data.Tuple (Tuple(..))

import UIGuide.Components.Button as Button
import UIGuide.Components.FormControl as FormControl
import UIGuide.Components.TextFields as TextFields

routes = SM.fromFoldable
  [ Tuple "textfields"
      { anchor: "Text Fields", component: proxy TextFields.component }
  , Tuple "buttons"
      { anchor: "Buttons", component: proxy Button.component }
  , Tuple "formcontrols"
      { anchor: "Form Controls", component: proxy FormControl.component }
  ]
