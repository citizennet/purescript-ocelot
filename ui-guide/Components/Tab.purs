module UIGuide.Components.Tab where

import Prelude

import Ocelot.Block.NavigationTab as NavigationTab
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import UIGuide.Block.Component as Component
import UIGuide.Block.Documentation as Documentation

type State = Unit

data Query a = NoOp a

type Input = Unit

type Message = Void


component :: ∀ m. H.Component HH.HTML Query Input Message m
component =
  H.component
    { initialState: const unit
    , render
    , eval
    , receiver: const Nothing
    }
  where
    eval :: Query ~> H.ComponentDSL State Query Message m
    eval = case _ of
      NoOp a -> do
        pure a

    render :: State -> H.ComponentHTML Query
    render _ =
      HH.div_
      [ Documentation.documentation
          { header: "Tabs"
          , subheader: "Tabs for navigating, eg. between form pages"
          }
          [ Component.component
              { title: "Tabs" }
              [ HH.div
                  [ HP.class_ (HH.ClassName "bg-black-10 flex items-center justify-center h-full w-full") ]
                  [ NavigationTab.navigationTabs (tabConfig defaultTabs) ]
              ]
          , Component.component
              { title: "Tabs with Errors" }
              [ HH.div
                  [ HP.class_ (HH.ClassName "bg-black-10 flex items-center justify-center h-full w-full") ]
                  [ NavigationTab.navigationTabs (tabConfig errorTabs) ]
              ]
          ]
      ]
      where
        defaultTabs :: Array (NavigationTab.Tab Boolean)
        defaultTabs =
          [ { name: "Accounts & Spend", link: "#", page: true, errors: 0 }
          , { name: "Automatic Optimization", link: "#", page: false, errors: 0 }
          , { name: "Creative", link: "#", page: false, errors: 0 }
          ]

        errorTabs :: Array (NavigationTab.Tab Boolean)
        errorTabs =
          [ { name: "Accounts & Spend", link: "#", page: true, errors: 0 }
          , { name: "Automatic Optimization", link: "#", page: false, errors: 1 }
          , { name: "Creative", link: "#", page: false, errors: 0 }
          ]

        tabConfig :: Array (NavigationTab.Tab Boolean) -> NavigationTab.TabConfig Boolean
        tabConfig tabs =
          { tabs: tabs
          , activePage: true
          }
