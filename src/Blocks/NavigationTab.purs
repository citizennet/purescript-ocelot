module Ocelot.Block.NavigationTab where

import Prelude

import Data.Array ((:))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ocelot.Block.Icon (info)

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

outerClasses :: Array HH.ClassName
outerClasses = HH.ClassName <$>
  [ "bg-black-10"
  , "w-full"
  , "px-6"
  ]

innerClasses :: Array HH.ClassName
innerClasses = HH.ClassName <$>
  [ "container"
  , "items-end"
  , "mx-auto"
  , "flex"
  , "px-20"
  , "h-16"
  , "list-reset"
  ]

tabClasses :: Array HH.ClassName
tabClasses = HH.ClassName <$>
  [ "pt-5"
  , "pb-6"
  , "inline-block"
  , "no-underline"
  ]

activeTabClasses :: Array HH.ClassName
activeTabClasses = HH.ClassName <$>
  [ "border-b-2"
  , "border-blue-88"
  , "text-white"
  ]

inactiveTabClasses :: Array HH.ClassName
inactiveTabClasses = HH.ClassName <$>
  [ "border-b-2"
  , "border-black-10"
  , "hover:border-blue-88"
  , "hover:text-white"
  , "text-grey-70"
  ]

tabTextClasses :: Array HH.ClassName
tabTextClasses = HH.ClassName <$>
  [ "text-sm"
  , "tracking-wide"
  , "uppercase"
  , "bold"
  , "inline-flex"
  ]

errorIconClasses :: Array HH.ClassName
errorIconClasses = HH.ClassName <$>
  [ "text-2xl"
  , "text-red"
  , "mr-4"
  , "inline-flex"
  , "align-bottom"
  ]

navigationTabs
  :: ∀ p i page
   . Eq page
  => TabConfig page
  -> HH.HTML p i
navigationTabs { tabs, activePage } =
  HH.div
    [ HP.classes outerClasses ]
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
      , HP.classes $ tabClasses <> conditionalTabClasses isActive
      ]
      [ info
        [ HP.classes $ conditionalIconClasses isActive tab.errors ]
      , HH.span
        [ HP.classes tabTextClasses ]
        [ HH.text tab.name ]
      ]
    ]
  where
    isActive :: Boolean
    isActive = tab.page == activePage

    conditionalTabClasses :: IsActive -> Array HH.ClassName
    conditionalTabClasses true = activeTabClasses
    conditionalTabClasses false = inactiveTabClasses

    conditionalIconClasses :: IsActive -> Int -> Array HH.ClassName
    conditionalIconClasses active errors
      | errors > 0 && active == false = errorIconClasses
      | otherwise = HH.ClassName "hidden" : errorIconClasses
