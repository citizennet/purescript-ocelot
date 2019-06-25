module Ocelot.Block.NavigationTab where

import Prelude

import DOM.HTML.Indexed (HTMLdiv)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ocelot.Block.TabControl as TabControl

type Tab page =
  { name :: String
  , link :: String
  , page :: page
  , errors :: Int
  }

type TabConfig page =
  { tabs :: Array (Tab page)
  , activePage :: page
  }

navigationTabs
  :: ∀ p i page
   . Eq page
  => TabConfig page
  -> Array (HH.IProp HTMLdiv i)
  -> HH.HTML p i
navigationTabs { tabs, activePage } props =
  TabControl.tabControl config props
  where
    config =
      { tabs: toGeneric <$> tabs, activePage }

    toGeneric tab =
      { name: tab.name
      , props: [ HP.href tab.link ]
      , page: tab.page
      , errors: tab.errors
      }

navigationTabs_
  :: ∀ p i page
   . Eq page
  => TabConfig page
  -> HH.HTML p i
navigationTabs_ config = navigationTabs config []
