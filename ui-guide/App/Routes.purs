module UIGuide.App.Routes
  ( routes, groups )
where

import Data.Map as M
import Data.Tuple (Tuple(..))
import UIGuide.App (Group(..), proxy)
import UIGuide.Components.Button as Button
import UIGuide.Components.Card as Card
import UIGuide.Components.FormControl as FormControl
import UIGuide.Components.Tab as Tab
import UIGuide.Components.TextFields as TextFields
import UIGuide.Components.Validation as Validation

----------
-- Routes

groups =
  [ Components
  , FormElements
  , Behaviors
  ]

routes = M.fromFoldable
  [ Tuple "tabs"
    { anchor: "Tabs", component: proxy Tab.component, group: Components }
  , Tuple "cards"
    { anchor: "Cards", component: proxy Card.card, group: Components }
  , Tuple "textfields"
      { anchor: "Text Fields", component: proxy TextFields.component, group: FormElements }
  , Tuple "buttons"
      { anchor: "Buttons", component: proxy Button.component, group: FormElements }
  , Tuple "formcontrols"
      { anchor: "Form Controls", component: proxy FormControl.component, group: FormElements }
  , Tuple "validation"
      { anchor: "Validation", component: proxy Validation.component, group: Behaviors }
  ]
