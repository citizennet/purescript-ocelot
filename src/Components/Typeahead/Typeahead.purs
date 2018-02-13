module CN.UI.Components.Typeahead where

import Prelude

import Control.Monad.Aff.Class (class MonadAff)
import Network.RemoteData (RemoteData(NotAsked))
import Data.Array (dropEnd, mapWithIndex, takeEnd)
import Data.Foldable (foldr)
import Data.StrMap (StrMap)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.String (Pattern(Pattern), split)
import Data.Time.Duration (Milliseconds(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Component.ChildPath as CP

import Select.Primitives.Container as C
import Select.Primitives.Search as S

import CN.UI.Core.Typeahead as TA
import CN.UI.Block.Input as Input


----------
-- Default typeahead configurations

-- A default single-select that is provided with a renderItem function.
defaultSingle :: ∀ o item source err eff m
  . MonadAff (TA.Effects eff) m
  => TA.CompareToString item
  => Eq item
  => Show err
  => Array item
  -> (String -> (Maybe Int) -> Int -> item -> H.HTML Void (C.ContainerQuery o item))
  -> TA.TypeaheadInput o item source err (TA.Effects eff) m
defaultSingle xs renderItem =
  { items: TA.Sync xs
  , search: Nothing
  , initialSelection: TA.One Nothing
  , render: renderTA renderItem
  , config: defaultConfig
  }

-- A default multi-select limited to N total possible selections.
defaultLimit :: ∀ o item source err eff m
  . MonadAff (TA.Effects eff) m
  => TA.CompareToString item
  => Eq item
  => Show err
  => Int
  -> Array item
  -> (String -> (Maybe Int) -> Int -> item -> H.HTML Void (C.ContainerQuery o item))
  -> TA.TypeaheadInput o item source err (TA.Effects eff) m
defaultLimit n xs renderItem =
  { items: TA.Sync xs
  , search: Nothing
  , initialSelection: TA.Limit n []
  , render: renderTA renderItem
  , config: defaultConfig
  }

-- A default multi-select that is provided with a renderItem function to determine
-- rendering a specific item in the container
defaultMulti :: ∀ o item source err eff m
  . MonadAff (TA.Effects eff) m
  => TA.CompareToString item
  => Eq item
  => Show err
  => Array item
  -> (String -> (Maybe Int) -> Int -> item -> H.HTML Void (C.ContainerQuery o item))
  -> TA.TypeaheadInput o item source err (TA.Effects eff) m
defaultMulti xs renderItem =
  { items: TA.Sync xs
  , search: Nothing
  , initialSelection: TA.Many []
  , render: renderTA renderItem
  , config: defaultConfig
  }

-- A default multi-select using the default render item function
defaultMulti' :: ∀ o item source err eff m
  . MonadAff (TA.Effects eff) m
  => TA.CompareToString item
  => Eq item
  => Show err
  => (item -> StrMap String)
  -> Array item
  -> TA.TypeaheadInput o item source err (TA.Effects eff) m
defaultMulti' toStrMap xs =
  { items: TA.Sync xs
  , search: Nothing
  , initialSelection: TA.Many []
  , render: renderTA defaultRenderItem
  , config: defaultFuzzyConfig toStrMap
  }

-- A default multi-select using the default render item function
defaultAsyncMulti' :: ∀ o item source err eff m
  . MonadAff (TA.Effects eff) m
  => TA.CompareToString item
  => Eq item
  => Show err
  => source
  -> TA.TypeaheadInput o item source err (TA.Effects eff) m
defaultAsyncMulti' source =
  { items: TA.Async source NotAsked
  , search: Nothing
  , initialSelection: TA.Many []
  , render: renderTA defaultRenderItem
  , config: defaultConfig
  }

-- A continuous asynchronous typeahead, reasonably debounced and
-- not filtered.
defaultContAsyncMulti' :: ∀ o item source err eff m
  . MonadAff (TA.Effects eff) m
 => TA.CompareToString item
 => Eq item
 => Show err
 => source
 -> TA.TypeaheadInput o item source err (TA.Effects eff) m
defaultContAsyncMulti' source =
  { items: TA.ContinuousAsync (Milliseconds 500.0) "" source NotAsked
  , search: Nothing
  , initialSelection: TA.Many []
  , render: renderTA defaultRenderItem
  , config: contAsyncConfig
  }


----------
-- Default Configuration

defaultConfig :: ∀ item
  . TA.CompareToString item
 => Eq item
 => TA.Config item
defaultConfig =
  { insertable: TA.NotInsertable
  , filterType: TA.CaseInsensitive
  , keepOpen: true
  }

-- toStrMap should take an item and produce a string map
-- include the keys / values you want to filter against
-- https://github.com/citizennet/purescript-fuzzy/blob/develop/test/Main.purs
defaultFuzzyConfig :: ∀ item
  . TA.CompareToString item
 => Eq item
 => (item -> StrMap String)
 -> TA.Config item
defaultFuzzyConfig toStrMap =
  { insertable: TA.NotInsertable
  , filterType: TA.FuzzyMatch
  , keepOpen: true
  , toStrMap
  }

contAsyncConfig :: ∀ item
  . TA.CompareToString item
 => Eq item
 => TA.Config item
contAsyncConfig =
  { insertable: TA.NotInsertable
  , filterType: TA.CaseInsensitive
  , keepOpen: true
  }


----------
-- Render functions

renderTA :: ∀ o item source err eff m
  . MonadAff (TA.Effects eff) m
 => TA.CompareToString item
 => Eq item
 => (String -> (Maybe Int) -> Int -> item -> H.HTML Void (C.ContainerQuery o item))
 -> TA.TypeaheadState item source err
 -> H.ParentHTML (TA.TypeaheadQuery o item source err (TA.Effects eff) m) (TA.ChildQuery o item (TA.Effects eff)) TA.ChildSlot m
renderTA renderItem st = HH.span
  [ HP.class_ $ HH.ClassName "w-full" ]
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
      { render: renderContainer st, items: fromMaybe [] $ TA.maybeUnpackItems st.items }
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
            [ HP.class_ $ HH.ClassName "bg-white rounded-sm w-full text-grey-darkest border-b border-grey-lighter" ]
            [ HH.ul
              [ HP.class_ $ HH.ClassName "list-reset" ]
              $ renderSelection <$> xs
            ]
          ]
      where
        renderSelection item =
          HH.li
          [ HP.class_ $ HH.ClassName "px-4 py-1 hover:bg-grey-lighter relative" ]
          [ HH.span_
            [ HH.text $ TA.compareToString item ]
          , HH.span
            [ HP.class_ $ HH.ClassName "absolute pin-t pin-b pin-r p-1 mx-3 cursor-pointer"
            , HE.onClick (HE.input_ (TA.Remove item)) ]
            [ HH.text "×" ]
          ]

    renderContainer parentState containerState = HH.div [ HP.class_ $ HH.ClassName "relative" ]
      if not containerState.open
        then []
        else [ HH.div
          ( C.getContainerProps
            [ HP.class_ $ HH.ClassName "absolute bg-white shadow max-h-80 overflow-y-scroll rounded-sm pin-t pin-l w-full" ]
          )
          [ HH.ul
            [ HP.class_ $ HH.ClassName "list-reset" ]
            $ renderItem (parentState.search) containerState.highlightedIndex `mapWithIndex` containerState.items
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


defaultRenderItem :: forall o item. TA.CompareToString item => String -> Maybe Int -> Int -> item -> HH.HTML Void (C.ContainerQuery o item Unit)
defaultRenderItem "" highlightIndex itemIndex item = HH.li
  ( C.getItemProps itemIndex
    [ HP.class_ $ HH.ClassName $ "px-4 py-1 text-grey-darkest" <> hover ]
  )
    [ HH.text $ TA.compareToString item ]
  where
    hover = if highlightIndex == Just itemIndex then " bg-grey-lighter" else ""
defaultRenderItem search highlightIndex itemIndex item = HH.li
  ( C.getItemProps itemIndex
    [ HP.class_ $ HH.ClassName $ "px-4 py-1 text-grey-darkest" <> hover ]
  )
  ( boldMatches (Pattern search) $ TA.compareToString item )
  where
    hover = if highlightIndex == Just itemIndex then " bg-grey-lighter" else ""


-- Wrap matching text in a bold span when the user performs a search
boldMatches :: ∀ i p. Pattern -> String -> Array (H.HTML i p)
boldMatches p@(Pattern p') src = html <> lastHtml
  where
    bold = HH.span [ HP.class_ $ HH.ClassName "font-bold" ] [ HH.text p' ]
    alreadySplit = split p src
    html = foldr (\text acc -> [ HH.text text, bold ] <> acc) [] (dropEnd 1 alreadySplit)
    lastHtml = map HH.text $ takeEnd 1 alreadySplit
