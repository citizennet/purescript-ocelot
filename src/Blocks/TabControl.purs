module Ocelot.Block.TabControl where

import Prelude

import DOM.HTML.Indexed (HTMLdiv, HTMLa)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ocelot.Block.Icon as Icon
import Ocelot.HTML.Properties ((<&>))

type Tab i page =
  { name :: String
  , props :: Array (HH.IProp HTMLa i)
  , page :: page
  , errors :: Int
  }

type TabConfig tabType page =
  { tabs :: Array (tabType page)
  , activePage :: page
  }

type IsActive = Boolean

outerClasses :: Array HH.ClassName
outerClasses = HH.ClassName <$>
  [ "bg-black-10"
  , "w-full"
  ]

innerClasses :: Array HH.ClassName
innerClasses = HH.ClassName <$>
  [ "container"
  , "items-end"
  , "mx-auto"
  , "flex"
  , "h-16"
  , "list-reset"
  ]

tabClasses :: Array HH.ClassName
tabClasses = HH.ClassName <$>
  [ "pt-5"
  , "pb-6"
  , "inline-flex"
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
  , "self-end"
  ]

errorIconClasses :: Array HH.ClassName
errorIconClasses = HH.ClassName <$>
  [ "text-2xl"
  , "text-red"
  , "mr-1"
  , "inline-flex"
  , "align-bottom"
  , "my-px"
  ]

tabControl
  :: ∀ p i page
   . Eq page
  => TabConfig (Tab i) page
  -> Array (HH.IProp HTMLdiv i)
  -> HH.HTML p i
tabControl { tabs, activePage } props =
  HH.div
    [ HP.classes outerClasses ]
    [ HH.ul
      ( [ HP.classes innerClasses ] <&> props )
      $ singleTab activePage <$> tabs
    ]

tabControl_
  :: ∀ p i page
   . Eq page
  => TabConfig (Tab i) page
  -> HH.HTML p i
tabControl_ config = tabControl config []

singleTab
  :: ∀ p i page
   . Eq page
  => page
  -> Tab i page
  -> HH.HTML p i
singleTab activePage tab =
  HH.li
    [ HP.class_ $ HH.ClassName "mr-12" ]
    [ HH.a
      ( tab.props
        <> [ HP.classes $ tabClasses <> conditionalTabClasses isActive ]
      )
      ( errorIcon
        <>
        [ HH.span
          [ HP.classes tabTextClasses ]
          [ HH.text tab.name ]
        ]
      )
    ]
  where
    isActive :: Boolean
    isActive = tab.page == activePage

    errorIcon :: Array (HH.HTML p i)
    errorIcon
      | tab.errors > 0 && isActive == false =
        [ Icon.error
          [ HP.classes $ errorIconClasses ]
        ]
      | otherwise = []

    conditionalTabClasses :: IsActive -> Array HH.ClassName
    conditionalTabClasses true = activeTabClasses
    conditionalTabClasses false = inactiveTabClasses
