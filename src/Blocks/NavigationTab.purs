module CN.UI.Block.NavigationTab where

import Prelude

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

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

type IsActive = Boolean

tabClasses :: Array HH.ClassName
tabClasses = HH.ClassName <$>
  [ "leading-normal"
  , "mt-4"
  , "mr-3"
  , "no-underline"
  , "text-xs"
  , "tracking-wide"
  , "uppercase"
  ]

activeTabClasses :: Array HH.ClassName
activeTabClasses = HH.ClassName <$>
  [ "border-b-2"
  , "border-blue-88"
  , "text-white"
  ]

inactiveTabClasses :: Array HH.ClassName
inactiveTabClasses = HH.ClassName <$>
  [ "hover:border-b-2"
  , "hover:border-blue-88"
  , "hover:text-white"
  , "text-grey-70"
  ]

navigationTabs
  :: ∀ p i page
   . Eq page
  => TabConfig page
  -> HH.HTML p i
navigationTabs { tabs, activePage } =
  HH.div
    [ HP.class_ (HH.ClassName "flex w-full h-12 bg-black-10") ]
    $ navigationTab activePage <$> tabs

navigationTab
  :: ∀ p i page
   . Eq page
  => page
  -> Tab page
  -> HH.HTML p i
navigationTab activePage tab =
  HH.a
    [ HP.href tab.link
    , HP.classes $ tabClasses <> tabStyles (tab.page == activePage)
    ]
    $ (if tab.errors > 0 && tab.page /= activePage then [ HH.i [ HP.class_ $ HH.ClassName "mr-2"] [ HH.text "errors" ] ] else [])
    <> [ HH.text tab.name ]

  where
    tabStyles :: IsActive -> Array HH.ClassName
    tabStyles true = activeTabClasses
    tabStyles false = inactiveTabClasses
