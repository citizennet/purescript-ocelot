module Ocelot.Block.NavigationTab where

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

navClasses :: Array HH.ClassName
navClasses = HH.ClassName <$>
  [ "bg-black-10"
  , "px-8"
  , "w-full"
  ]

innerClasses :: Array HH.ClassName
innerClasses = HH.ClassName <$>
  [ "container"
  , "m-auto"
  , "flex"
  , "px-20"
  ]

tabClasses :: Array HH.ClassName
tabClasses = HH.ClassName <$>
  [ "leading-normal"
  , "py-6"
  , "inline-block"
  , "no-underline"
  , "tracking-wide"
  , "uppercase"
  , "text-sm"
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
    [ HP.classes navClasses ]
    [ HH.ul
      [ HP.classes innerClasses ]
      $ navigationTab activePage <$> tabs
    ]

navigationTab
  :: ∀ p i page
   . Eq page
  => page
  -> Tab page
  -> HH.HTML p i
navigationTab activePage tab =
  HH.li
    [ HP.class_ $ HH.ClassName "mr-12" ]
    [ HH.a
      [ HP.href tab.link
      , HP.classes $ tabClasses <> tabStyles (tab.page == activePage)
      ]
      $ (if tab.errors > 0 && tab.page /= activePage then [ HH.i [ HP.class_ $ HH.ClassName "mr-2"] [ HH.text "errors" ] ] else [])
      <> [ HH.text tab.name ]
    ]

  where
    tabStyles :: IsActive -> Array HH.ClassName
    tabStyles true = activeTabClasses
    tabStyles false = inactiveTabClasses
