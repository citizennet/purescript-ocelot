module Ocelot.Block.Header where

import Prelude

import DOM.HTML.Indexed (HTMLdiv)
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ocelot.Block.Layout as Layout
import Ocelot.Block.NavigationTab as NavigationTab
import Ocelot.HTML.Properties (css, (<&>))

outerClasses :: Array HH.ClassName
outerClasses = HH.ClassName <$>
  [ "bg-black-10"
  , "w-full"
  , "px-6"
  ]

innerClasses :: Array HH.ClassName
innerClasses = HH.ClassName <$>
  [ "container"
  , "items-center"
  , "mx-auto"
  , "flex"
  , "h-24"
  ]

header
  :: ∀ p i
   . Array (HH.IProp HTMLdiv i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
header iprops =
  HH.header [ HP.classes outerClasses ]
  <<< pure
  <<< HH.div ( [ HP.classes innerClasses ] <&> iprops )

header_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
header_ = header []

type FormHeaderProps p i =
  { buttons :: Array (HH.HTML p i)
  , name :: Array (HH.HTML p i)
  , title :: Array (HH.HTML p i)
  , brand :: Maybe String
  }

stickyFormHeader
  :: ∀ p i page
   . Eq page
  => FormHeaderProps p i
  -> NavigationTab.TabConfig page
  -> HH.HTML p i
stickyFormHeader hConfig tConfig =
  HH.div
    [ HP.class_ $ HH.ClassName "h-40" ]
    [ HH.div
      [ HP.classes Layout.stickyClasses ]
      [ formHeader hConfig
      , NavigationTab.navigationTabs tConfig [ HP.class_ $ HH.ClassName "px-16" ]
      ]
    ]

stickyHeader_ :: ∀ p i. FormHeaderProps p i -> HH.HTML p i
stickyHeader_ config =
  HH.div
    [ HP.class_ $ HH.ClassName "h-20" ]
    [ HH.div
      [ HP.classes Layout.stickyClasses ]
      [ formHeader config ]
    ]

formHeader :: ∀ p i. FormHeaderProps p i -> HH.HTML p i
formHeader props =
  header_ $
    case props.brand of
      Just src  ->
        [ HH.div
          [ css "w-16" ]
          [ HH.img [ HP.src src ] ]
        ]
      Nothing -> []
    <>
    [ HH.h2
      [ css "flex-1 font-medium" ]
        [ HH.span
            [ css "text-lg text-grey-70 mr-4" ]
            props.name
        , HH.span
            [ css "text-lg text-white" ]
            props.title
        ]
    ]
    <>
    props.buttons
