module UIGuide.App.Routes
  ( routes
  , groups
  ) where

import Prelude

import Data.Const (Const)
import Data.Map (Map, fromFoldable)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Halogen as H
import UIGuide.App (Group(..), proxy)
import UIGuide.Component.Badge as Badge
import UIGuide.Component.Button as Button
import UIGuide.Component.DatePickers as DatePickers
import UIGuide.Component.Diagram as Diagram
import UIGuide.Component.Dialogs as Dialogs
import UIGuide.Component.Dropdown as Dropdown
import UIGuide.Component.ExpansionCards as ExpansionCards
import UIGuide.Component.FilePicker as FilePicker
import UIGuide.Component.FormControl as FormControl
import UIGuide.Component.Icons as Icons
import UIGuide.Component.Modals as Modals
import UIGuide.Component.MultiInput as MultiInput
import UIGuide.Component.Table as Table
import UIGuide.Component.TextFields as TextFields
import UIGuide.Component.Tray as Tray
import UIGuide.Component.Type as Type
import UIGuide.Component.Typeaheads as Typeaheads

----------
-- Routes

groups :: Array Group
groups =
  [ Basics
  , Components
  ]

type RouteConfig =
  { anchor :: String
  , component :: H.Component (Const Void) Unit Void Aff
  , group :: Group
  }

routes :: Map String RouteConfig
routes = fromFoldable
  -- [ Tuple "tabs"
  -- { anchor: "Tabs"
  -- , component: proxy Tab.component
  -- , group: FormElements
  -- }
  [ Tuple "expandables"
      { anchor: "Expansion Cards"
      , component: proxy ExpansionCards.component
      , group: Components
      }
  , Tuple "textfields"
      { anchor: "Text Fields"
      , component: proxy TextFields.component
      , group: Components
      }
  , Tuple "typeaheads"
      { anchor: "Typeaheads"
      , component: proxy Typeaheads.component
      , group: Components
      }
  , Tuple "date-pickers"
      { anchor: "Date Pickers"
      , component: proxy DatePickers.component
      , group: Components
      }
  , Tuple "buttons"
      { anchor: "Buttons"
      , component: proxy Button.component
      , group: Components
      }
  , Tuple "controls"
      { anchor: "Controls"
      , component: proxy FormControl.component
      , group: Components
      }
  , Tuple "modals"
      { anchor: "Modals"
      , component: proxy Modals.component
      , group: Components
      }
  , Tuple "tables"
      { anchor: "Tables"
      , component: proxy Table.component
      , group: Components
      }
  , Tuple "dropdowns"
      { anchor: "Dropdowns"
      , component: proxy Dropdown.component
      , group: Components
      }
  , Tuple "type"
      { anchor: "Type"
      , component: proxy Type.component
      , group: Basics
      }
  , Tuple "icons"
      { anchor: "Icons"
      , component: proxy Icons.component
      , group: Basics
      }
  , Tuple "tray"
      { anchor: "Tray"
      , component: proxy Tray.component
      , group: Components
      }
  , Tuple "dialogs"
      { anchor: "Dialogs"
      , component: proxy Dialogs.component
      , group: Components
      }
  , Tuple "badge"
      { anchor: "Badge"
      , component: proxy Badge.component
      , group: Basics
      }
  , Tuple "multi-input"
      { anchor: "Multi Input"
      , component: proxy MultiInput.component
      , group: Components
      }
  , Tuple "diagram"
      { anchor: "Diagram"
      , component: proxy Diagram.component
      , group: Basics
      }
  , Tuple "file-picker"
      { anchor: "File Picker"
      , component: proxy FilePicker.component
      , group: Components
      }
  ]
