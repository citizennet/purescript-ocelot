module CN.UI.Block.ItemContainer where

import Prelude

import Data.FunctorWithIndex (mapWithIndex)
import Data.Fuzzy (Fuzzy(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.StrMap (lookup)
import Data.Monoid (mempty)
import Data.Either (Either(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Select.Primitives.Container as C

baseClasses :: Array HH.ClassName
baseClasses = HH.ClassName <$>
  [ "bg-white"
  , "rounded-sm"
  , "w-full" ]

selectionContainerClasses :: Array HH.ClassName
selectionContainerClasses = baseClasses <> names
  where
    names = HH.ClassName <$>
      [ "border-b"
      , "border-grey-lighter"
      , "px-1"
      , "py-1"
      ]

itemContainerClasses :: Array HH.ClassName
itemContainerClasses = baseClasses <> names
  where
    names = HH.ClassName <$>
      [ "absolute"
      , "shadow"
      , "max-h-80"
      , "overflow-y-scroll"
      , "pin-t"
      , "pin-l"
      , "z-50"
      ]

ulClasses :: Array HH.ClassName
ulClasses = HH.ClassName <$> [ "list-reset" ]

liClasses :: Array HH.ClassName
liClasses = HH.ClassName <$>
  [ "px-3"
  , "py-2"
  , "rounded-sm"
  , "text-grey-darkest"
  , "group"
  , "leading-normal"
  , "hover:bg-grey-lighter"
  ]

buttonClasses :: Array HH.ClassName
buttonClasses = HH.ClassName <$>
  [ "float-right"
  , "leading-normal"
  , "invisible"
  , "group-hover:visible"
  ]

-- Provided an array of items, renders them in a container.
-- Items should have already been treated with `boldMatches` by this point.
itemContainer
  :: ∀ t o item
   . Maybe Int
  -> Array HH.PlainHTML
  -> HH.HTML t (C.ContainerQuery o item Unit)
itemContainer highlightIndex html =
  HH.div
  (C.getContainerProps [ HP.classes itemContainerClasses ])
  [ HH.ul
    [ HP.classes ulClasses ]
    $ mapWithIndex
      ( \i h ->
          HH.li
            ( C.getItemProps i
              [ HP.classes (liClasses <> hover i) ]
            )
            [ HH.fromPlainHTML h ]
      )
      html
  ]
  where
    hover i = if highlightIndex == Just i then HH.ClassName <$> [ "bg-grey-lighter" ] else mempty


-- Provided an array of selection items, renders them in a container
-- Make sure the array of items includes the correct click handlers:w
selectionContainer :: ∀ p i. Array (HH.HTML p i) -> HH.HTML p i
selectionContainer html =
  HH.div
  [ HP.classes selectionContainerClasses ]
  [ HH.ul
    [ HP.classes ulClasses ]
    $ html <#>
    ( \h ->
        HH.li
          [ HP.classes liClasses ]
          [ h ]
    )
  ]


selectionGroup
  :: ∀ item i p
   . (item -> HH.PlainHTML)
  -> Array (HH.IProp _ p)
  -> item
  -> HH.HTML i p
selectionGroup f props item =
  HH.div
    props
    [ HH.fromPlainHTML (f item)
    , HH.button
      [ HP.classes buttonClasses ]
      [ HH.text "✕" ]
    ]


-- Takes a key to the segment that you want to display highlighted.
-- WARN: If the key you provided does not exist in the map, your item will not be
-- rendered!
boldMatches :: ∀ item i p. String -> Fuzzy item -> Array (HH.HTML i p)
boldMatches key (Fuzzy { segments }) = boldMatch <$> (fromMaybe [ Left key ] $ lookup key segments)
  where
    boldMatch (Left str) = HH.text str
    boldMatch (Right str) = HH.span [ HP.class_ $ HH.ClassName "font-bold" ] [ HH.text str ]
