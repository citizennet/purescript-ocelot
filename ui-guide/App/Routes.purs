module UIGuide.App.Routes
  ( routes, groups )
where


import Data.Map as M
import Data.Tuple (Tuple(..))

import UIGuide.App (Group(Fundamental, Primitive), proxy)

import UIGuide.Components.Button as Button
import UIGuide.Components.FormControl as FormControl
import UIGuide.Components.TextFields as TextFields


----------
-- Routes

groups = [ Primitive, Fundamental ]

routes = M.fromFoldable
  [ Tuple "textfields"
      { anchor: "Text Fields", component: proxy TextFields.component, group: Primitive }
  , Tuple "buttons"
      { anchor: "Buttons", component: proxy Button.component, group: Primitive }
  , Tuple "formcontrols"
      { anchor: "Form Controls", component: proxy FormControl.component, group: Fundamental }
  ]
