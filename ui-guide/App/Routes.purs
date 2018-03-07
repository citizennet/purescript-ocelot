module UIGuide.App.Routes
  ( routes, groups )
where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff.Timer (TIMER)
import DOM (DOM)
import Data.Const (Const)
import Data.Map (Map, fromFoldable)
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.Storybook.Proxy (ProxyS)
import Network.HTTP.Affjax (AJAX)
import UIGuide.App (Group(..), proxy)
import UIGuide.Components.Button as Button
import UIGuide.Components.Card as Card
import UIGuide.Components.FormControl as FormControl
import UIGuide.Components.Icon as Icon
import UIGuide.Components.Tab as Tab
import UIGuide.Components.TextFields as TextFields
import UIGuide.Components.Validation as Validation

----------
-- Routes

groups :: Array Group
groups =
  [ Components
  , FormElements
  , Behaviors
  ]

type RouteEffects eff =
  ( console :: CONSOLE
  , ajax :: AJAX
  , avar :: AVAR
  , dom :: DOM
  , timer :: TIMER
  | eff
  )

type RouteConfig eff =
  { anchor :: String
  , component :: H.Component HH.HTML (ProxyS (Const Void) Unit) Unit Void (Aff (RouteEffects eff))
  , group :: Group
  }

routes :: ∀ eff. Map String (RouteConfig eff)
routes = fromFoldable
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
  , Tuple "icon"
      { anchor: "Icon", component: proxy Icon.icon, group: FormElements }
  ]
