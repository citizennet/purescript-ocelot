module UIGuide.Blocks.Sidebar where

import Prelude ((<$>), (<<<))

import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

css :: forall e p. String -> H.IProp ( "class" :: String | e ) p
css = HP.class_ <<< HH.ClassName

sidebar :: ∀ i p. Array (Tuple String (Array (Tuple String String))) -> H.HTML i p
sidebar navSections =
  HH.div
  [ HP.id_ "sidebar"
  , css "hidden z-50 fixed pin-y pin-l overflow-y-scroll md:overflow-visible scrolling-touch md:scrolling-auto bg-grey-lighter w-4/5 md:w-full md:max-w-xs flex-none border-r-2 border-grey-light md:flex flex-col" ]
  [ HH.div
    [ css "p-8 flex-1 overflow-y-scroll" ]
    [ HH.nav
      [ HP.id_ "nav"
      , css "text-base overflow-y-scroll" ]
      ( navSection <$> navSections )
    ]
  ]
  where
    navSection :: Tuple Title (Array (Tuple Anchor Link)) -> H.HTML i p
    navSection (Tuple title items) =
      HH.div
      [ css "mb-8" ]
      [ HH.p
        [ css "mb-4 text-grey-dark uppercase tracking-wide font-bold text-xs" ]
        [ HH.text title ]
      , HH.ul
        [ css "list-reset" ]
        ( navLink <$> items )
      ]
      where
        navLink (Tuple anchor link) =
          HH.li
          [ css "mb-3" ]
          [ HH.a
            [ css "hover:underline text-grey-darkest"
            , HP.href link ]
            [ HH.text anchor ]
          ]

type Title = String
type Anchor = String
type Link = String

cnNavSections :: Array (Tuple Title (Array (Tuple Anchor Link)))
cnNavSections = [ intro, atoms, components, blocks ]
  where
    mkNavSection :: Title -> Array Anchor -> Tuple Title (Array (Tuple Anchor Link))
    mkNavSection title xs = Tuple title links
      where
        links :: Array (Tuple Anchor Link)
        links = (\a -> Tuple a "#") <$> xs

    intro = mkNavSection "Introduction" [ "CitizenNet UI Guide" ]

    atoms = mkNavSection "Atoms"
      [ "Grid"
      , "Typography"
      , "Colors" ]

    blocks = mkNavSection "Blocks"
      [ "Frame"
      , "Text Fields"
      , "Buttons"
      , "Dialogs"
      , "Hovercards"
      , "Filters"
      , "Sorting"
      , "Modals"
      , "Menus"
      , "Toasts"
      , "Paging"
      , "Loading"
      , "Validation" ]

    components = mkNavSection "Components"
      [ "Dropdown"
      , "Typeahead"
      , "Date Picker"
      , "Image Picker" ]

