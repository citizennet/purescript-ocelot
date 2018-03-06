module Ocelot.Block.FormHeader where

import Prelude

import DOM.Event.Types (MouseEvent)
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Ocelot.Block.Button as Button
import Ocelot.Block.Layout as Layout
import Ocelot.Block.NavigationTab as NavigationTab

type FormHeaderProps i =
  { onClick :: (MouseEvent -> Maybe i)
  , name :: String
  , title :: String
  , brand :: Maybe String
  }

outerClasses :: Array HH.ClassName
outerClasses = HH.ClassName <$>
  [ "bg-black-10"
  ]

innerClasses :: Array HH.ClassName
innerClasses = HH.ClassName <$>
  [ "container"
  , "items-center"
  , "mx-auto"
  , "flex"
  , "h-20"
  ]

formHeader
  :: ∀ p i page
   . Eq page
  => FormHeaderProps i
  -> NavigationTab.TabConfig page
  -> HH.HTML p i
formHeader props config =
  HH.div
    [ HP.class_ $ HH.ClassName "h-40" ]
    [ HH.div
      [ HP.classes Layout.headerClasses ]
      [ formHeaderInner props
      , NavigationTab.navigationTabs config
      ]
    ]

formHeader_ :: ∀ p i. FormHeaderProps i -> HH.HTML p i
formHeader_ props =
  HH.div
    [ HP.class_ $ HH.ClassName "h-20" ]
    [ HH.div
      [ HP.classes Layout.headerClasses ]
      [ formHeaderInner props ]
    ]

formHeaderInner :: ∀ p i. FormHeaderProps i -> HH.HTML p i
formHeaderInner props =
  HH.header
    [ HP.classes outerClasses ]
    [ HH.div
      [ HP.classes innerClasses ]
      ( case props.brand of
          Just src  ->
            [ HH.div
              [ HP.class_ (HH.ClassName "w-20") ]
              [ HH.img [ HP.src src ] ]
            ]
          otherwise -> []
        <>
        [ HH.h2
          [ HP.class_ (HH.ClassName "flex-1 font-medium") ]
            [ HH.span
                [ HP.class_ (HH.ClassName "text-lg text-grey-70 mr-4") ]
                [ HH.text props.name ]
            , HH.span
                [ HP.class_ (HH.ClassName "text-lg text-white") ]
                [ HH.text props.title ]
            ]
        , HH.span
          [ HP.class_( HH.ClassName "mr-2" ) ]
          [ Button.button
              { type_: Button.Transparent }
              []
              [ HH.text "Cancel" ]
          ]
        , Button.button
          { type_: Button.Primary }
          [ HE.onClick props.onClick ]
          [ HH.text "Create" ]
        ]
      )
    ]
