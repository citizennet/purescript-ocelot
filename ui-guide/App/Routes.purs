module UIGuide.App.Routes
  ( routes, groups )
where

import Prelude

import Effect.Aff (Aff)
import Data.Const (Const)
import Data.Map (Map, fromFoldable)
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.Storybook.Proxy (ProxyS)
import UIGuide.App (Group(..), proxy)
import UIGuide.Components.Button as Button
import UIGuide.Components.ExpansionCards as ExpansionCards
import UIGuide.Components.FormControl as FormControl
import UIGuide.Components.Icons as Icons
import UIGuide.Components.Modals as Modals
import UIGuide.Components.TextFields as TextFields
import UIGuide.Components.Type as Type
import UIGuide.Components.Typeaheads as Typeaheads
import UIGuide.Components.Validation as Validation

----------
-- Routes

groups :: Array Group
groups =
  [ Basics
  , Components
  , Behaviors
  ]

type RouteConfig =
  { anchor :: String
  , component :: H.Component HH.HTML (ProxyS (Const Void) Unit) Unit Void Aff
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
  , Tuple "validation"
    { anchor: "Validation"
    , component: proxy Validation.component
    , group: Behaviors
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
  ]
