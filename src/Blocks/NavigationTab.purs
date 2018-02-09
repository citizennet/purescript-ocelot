module CN.UI.Block.NavigationTab where

import Prelude

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type Tab page =
  { name :: String
  , link :: String
  , page :: page
  }

type TabConfig page =
  { tabs :: Array (Tab page)
  , activePage :: page
  }

type IsActive = Boolean

tabClasses :: Array HH.ClassName
tabClasses = HH.ClassName <$>
  [ "no-underline"
  , "uppercase"
  , "mt-4"
  , "mr-6"
  , "text-xs"
  , "tracking-wide"
  , "leading-normal"
  ]

activeTabClasses :: Array HH.ClassName
activeTabClasses = HH.ClassName <$>
  [ "border-b-2"
  , "border-blue"
  , "text-white"
  ]

inactiveTabClasses :: Array HH.ClassName
inactiveTabClasses = HH.ClassName <$>
  [ "text-grey"
  , "border-b-2"
  , "hover:border-blue"
  , "hover:text-white"
  ]


navigationTabs :: ∀ p i page. Eq page => TabConfig page -> HH.HTML p i
navigationTabs { tabs, activePage } =
  HH.div
    [ HP.class_ (HH.ClassName "flex h-12 bg-black") ]
    $ navigationTab activePage <$> tabs

navigationTab :: ∀ p i page. Eq page => page -> Tab page -> HH.HTML p i
navigationTab activePage tab =
  HH.a
    [ HP.href tab.link
    , HP.classes $ tabClasses <> tabStyles (tab.page == activePage)
    ]
    [ HH.text tab.name ]

  where
    tabStyles :: IsActive -> Array HH.ClassName
    tabStyles true = activeTabClasses
    tabStyles false = inactiveTabClasses
