module UIGuide.App.Routes
  ( routes, groups )
where


import Data.Map as M
import Data.Tuple (Tuple(..))

import UIGuide.App (Group(..), proxy)

import UIGuide.Components.Button as Button
import UIGuide.Components.FormControl as FormControl
import UIGuide.Components.Tab as Tab
import UIGuide.Components.TextFields as TextFields

----------
-- Routes

groups = [ FormElements ]

routes = M.fromFoldable
  [ Tuple "textfields"
      { anchor: "Text Fields", component: proxy TextFields.component, group: FormElements }
  , Tuple "buttons"
      { anchor: "Buttons", component: proxy Button.component, group: FormElements }
  , Tuple "formcontrols"
      { anchor: "Form Controls", component: proxy FormControl.component, group: FormElements }
  ]
