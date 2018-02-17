module CN.UI.Components.Typeahead where

import Prelude

import Data.Either (Either(..))
import Data.Fuzzy (Fuzzy(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.StrMap (StrMap, lookup)
import CN.UI.Core.Typeahead as TA
import Control.Monad.Aff.Class (class MonadAff)
import Data.Array (dropEnd, mapWithIndex, takeEnd)
import Data.Foldable (foldr)
import Data.Time.Duration (Milliseconds(..))
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..), withDefault)
import Select.Primitives.Container as C
import Select.Primitives.Search as S

import CN.UI.Block.Input as Input

----------
-- Default typeahead configurations

-- A default single-select that is provided with a renderFuzzy and renderItem function.
defaultSingle :: ∀ o item source err eff m
  . MonadAff (TA.Effects eff) m
 => Eq item
 => Show err
 => Array item
 -> (item -> StrMap String)
 -> RenderContainerRow o item
 -> (item -> TAParentHTML o item source err (TA.Effects eff) m)
 -> TA.TypeaheadInput o item source err (TA.Effects eff) m
defaultSingle xs toStrMap renderFuzzy renderItem =
  { items: TA.Sync xs
  , search: Nothing
  , initialSelection: TA.One Nothing
  , render: renderTA renderFuzzy renderItem
  , config: defaultConfig toStrMap
  }

-- A default multi-select limited to N total possible selections.
defaultLimit :: ∀ o item source err eff m
  . MonadAff (TA.Effects eff) m
 => Eq item
 => Show err
 => Int
 -> Array item
 -> (item -> StrMap String)
 -> RenderContainerRow o item
 -> (item -> TAParentHTML o item source err (TA.Effects eff) m)
 -> TA.TypeaheadInput o item source err (TA.Effects eff) m
defaultLimit n xs toStrMap renderFuzzy renderItem =
  { items: TA.Sync xs
  , search: Nothing
  , initialSelection: TA.Limit n []
  , render: renderTA renderFuzzy renderItem
  , config: defaultConfig toStrMap
  }

-- A default multi-select that is provided with a renderFuzzy and renderItem function to determine
-- rendering a specific item in the container
defaultMulti :: ∀ o item source err eff m
  . MonadAff (TA.Effects eff) m
 => Eq item
 => Show err
 => Array item
 -> (item -> StrMap String)
 -> RenderContainerRow o item
 -> (item -> TAParentHTML o item source err (TA.Effects eff) m)
 -> TA.TypeaheadInput o item source err (TA.Effects eff) m
defaultMulti xs toStrMap renderFuzzy renderItem =
  { items: TA.Sync xs
  , search: Nothing
  , initialSelection: TA.Many []
  , render: renderTA renderFuzzy renderItem
  , config: defaultConfig toStrMap
  }

-- A default async single select using the default render function
defaultAsyncSingle :: ∀ o item source err eff m
  . MonadAff (TA.Effects eff) m
  => Eq item
  => Show err
  => source
  -> (item -> StrMap String)
  -> RenderContainerRow o item
  -> (item -> TAParentHTML o item source err (TA.Effects eff) m)
  -> TA.TypeaheadInput o item source err (TA.Effects eff) m
defaultAsyncSingle source toStrMap renderFuzzy renderItem =
  { items: TA.Async source NotAsked
  , search: Nothing
  , initialSelection: TA.One Nothing
  , render: renderTA renderFuzzy renderItem
  , config: defaultConfig toStrMap
  }

-- A default multi-select using the default render item function
defaultAsyncMulti :: ∀ o item source err eff m
  . MonadAff (TA.Effects eff) m
 => Eq item
 => Show err
 => source
 -> (item -> StrMap String)
 -> RenderContainerRow o item
 -> (item -> TAParentHTML o item source err (TA.Effects eff) m)
 -> TA.TypeaheadInput o item source err (TA.Effects eff) m
defaultAsyncMulti source toStrMap renderFuzzy renderItem =
  { items: TA.Async source NotAsked
  , search: Nothing
  , initialSelection: TA.Many []
  , render: renderTA renderFuzzy renderItem
  , config: defaultConfig toStrMap
  }

-- A continuous asynchronous typeahead, reasonably debounced and
-- not filtered.
defaultContAsyncMulti :: ∀ o item source err eff m
  . MonadAff (TA.Effects eff) m
 => Eq item
 => Show err
 => source
 -> (item -> StrMap String)
 -> RenderContainerRow o item
 -> (item -> TAParentHTML o item source err (TA.Effects eff) m)
 -> TA.TypeaheadInput o item source err (TA.Effects eff) m
defaultContAsyncMulti source toStrMap renderFuzzy renderItem =
  { items: TA.ContinuousAsync (Milliseconds 500.0) "" source NotAsked
  , search: Nothing
  , initialSelection: TA.Many []
  , render: renderTA renderFuzzy renderItem
  , config: contAsyncConfig toStrMap
  }


----------
-- Default Configuration

defaultConfig :: ∀ item
  . Eq item
 => (item -> StrMap String)
 -> TA.Config item
defaultConfig toStrMap =
  { insertable: TA.NotInsertable
  , filterType: TA.FuzzyMatch
  , keepOpen: true
  , toStrMap
  }

-- toStrMap should take an item and produce a string map
-- include the keys / values you want to filter against
-- https://github.com/citizennet/purescript-fuzzy/blob/develop/test/Main.purs
defaultFuzzyConfig :: ∀ item
  . Eq item
 => (item -> StrMap String)
 -> TA.Config item
defaultFuzzyConfig toStrMap =
  { insertable: TA.NotInsertable
  , filterType: TA.FuzzyMatch
  , keepOpen: true
  , toStrMap
  }

contAsyncConfig :: ∀ item
  . Eq item
 => (item -> StrMap String)
 -> TA.Config item
contAsyncConfig toStrMap =
  { insertable: TA.NotInsertable
  , filterType: TA.FuzzyMatch
  , keepOpen: true
  , toStrMap
  }


type TAParentHTML o item source err eff m
  = H.ParentHTML (TA.TypeaheadQuery o item source err eff m) (TA.ChildQuery o (Fuzzy item) eff) TA.ChildSlot m

----------
-- Render functions

renderTA :: ∀ o item source err eff m
  . MonadAff (TA.Effects eff) m
 => Eq item
 => RenderContainerRow o item
 -> (item -> TAParentHTML o item source err (TA.Effects eff) m)
 -> TA.TypeaheadState item source err
 -> TAParentHTML o item source err (TA.Effects eff) m
renderTA renderFuzzy renderSelection st =
  HH.div
  [ HP.class_ $ HH.ClassName "w-full px-3" ]
  [ renderSelections
  , HH.slot'
      CP.cp2
      TA.SearchSlot
      S.component
      { render: renderSearch
      , search: Nothing
      , debounceTime: case st.items of
          (TA.ContinuousAsync db _ _ _) -> db
          _ -> Milliseconds 0.0
      }
      (HE.input TA.HandleSearch)
  , HH.slot'
      CP.cp1
      TA.ContainerSlot
      C.component
      { render: renderContainer st, items: [] }
      (HE.input TA.HandleContainer)
  ]
  where
    renderSelections =
      case st.selections of
        (TA.One Nothing) -> HH.div_ []
        (TA.One (Just x)) -> HH.div_
          [ HH.div
            [ HP.class_ $ HH.ClassName "bg-white rounded-sm w-full text-grey-darkest border-b border-grey-lighter" ]
            [ HH.ul
              [ HP.class_ $ HH.ClassName "list-reset" ]
              [ renderSelection x ]
            ]
          ]
        (TA.Many xs) -> HH.div_
          [ HH.div
            [ HP.class_ $ HH.ClassName "bg-white rounded-sm w-full text-grey-darkest border-b border-grey-lighter" ]
            [ HH.ul
              [ HP.class_ $ HH.ClassName "list-reset" ]
              $ renderSelection <$> xs
            ]
          ]
        (TA.Limit _ xs) -> HH.div_
          [ HH.div
            [ HP.class_ $ HH.ClassName "bg-white rounded-sm w-full text-grey-darkest border-b border-grey-lighter z-50" ]
            [ HH.ul
              [ HP.class_ $ HH.ClassName "list-reset" ]
              $ renderSelection <$> xs
            ]
          ]

    renderContainer parentSt containerState = HH.div [ HP.class_ $ HH.ClassName "relative" ]
      if not containerState.open
        then []
        else [ HH.div
          ( C.getContainerProps
            [ HP.class_ $ HH.ClassName "absolute bg-white shadow max-h-80 overflow-y-scroll rounded-sm pin-t pin-l w-full z-50" ]
          )
          [ HH.ul
            [ HP.class_ $ HH.ClassName "list-reset" ]
            ( renderFuzzy containerState.highlightedIndex `mapWithIndex` containerState.items )
          ]
        ]

    renderSearch searchState =
      HH.div
        [ HP.class_ $ HH.ClassName "flex items-center border-b-2" ]
        [ Input.input
          ( S.getInputProps
            [ HP.placeholder "Type to search..."
            , HP.value searchState.search
            ]
          )
        ]

defaultSelectionRow :: ∀ o item source err eff m
   . (item -> TAParentHTML o item source err eff m)
  -> item
  -> TAParentHTML o item source err eff m
defaultSelectionRow renderItem item =
  HH.li
    [ HP.class_ $ HH.ClassName "px-4 py-1 hover:bg-grey-lighter relative" ]
    [ HH.span_ $ [ renderItem item ]
    , HH.span
      [ HP.class_ $ HH.ClassName "absolute pin-t pin-b pin-r p-1 mx-3 cursor-pointer"
      , HE.onClick (HE.input_ (TA.Remove item)) ]
      [ HH.text "×" ]
    ]

type RenderContainerRow o item
   = Maybe Int
  -> Int
  -> Fuzzy item
  -> H.HTML Void (C.ContainerQuery o (Fuzzy item))

defaultContainerRow :: ∀ o item
   . (Fuzzy item -> Array (H.HTML Void (C.ContainerQuery o (Fuzzy item))))
  -> RenderContainerRow o item
defaultContainerRow renderFuzzy highlightIndex itemIndex item =
  HH.li
    ( C.getItemProps itemIndex
      [ HP.class_ $ HH.ClassName $ "px-4 py-1 text-grey-darkest" <> hover ]
    )
    ( renderFuzzy item )
  where
    hover = if highlightIndex == Just itemIndex then " bg-grey-lighter" else ""

-- Takes a key to the segment that you want to display highlighted.
-- WARN: If the key you provided does not exist in the map, your item will not be
-- rendered!
boldMatches :: ∀ item i p. String -> Fuzzy item -> Array (H.HTML i p)
boldMatches key (Fuzzy { segments }) = boldMatch <$> (fromMaybe [ Left key ] $ lookup key segments)
  where
    boldMatch (Left str) = HH.text str
    boldMatch (Right str) = HH.span [ HP.class_ $ HH.ClassName "font-bold" ] [ HH.text str ]
