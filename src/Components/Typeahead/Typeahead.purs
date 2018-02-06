module CN.UI.Components.Typeahead where

import Prelude

import Control.Monad.Aff.Class (class MonadAff)
import DOM (DOM)
import Control.Monad.Aff.AVar (AVAR)

import Network.RemoteData (RemoteData(..), withDefault)

import Data.Array (dropEnd, mapWithIndex, takeEnd)
import Data.Foldable (foldr)
import Data.Maybe (Maybe(Just, Nothing))
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

----------
-- Default typeahead

-- A default multi-select that is provided with a renderItem function to determine
-- rendering a specific item in the container
defaultMulti :: ∀ o item source err eff m
  . MonadAff ( dom :: DOM, avar :: AVAR | eff ) m
  => TA.CompareToString item
  => Eq item
  => Show err
  => Array item
  -> (String -> (Maybe Int) -> Int -> item -> H.HTML Void (C.ContainerQuery o item))
  -> TA.TypeaheadInput o item source err ( dom :: DOM, avar :: AVAR | eff ) m
defaultMulti xs renderItem =
  { items: TA.Sync xs
  , debounceTime: Milliseconds 0.0
  , search: Nothing
  , initialSelection: TA.Many []
  , render: renderTA renderItem
  , config: defaultConfig
  }

-- A default multi-select using the default render item function
defaultMulti' :: ∀ o item source err eff m
  . MonadAff ( dom :: DOM, avar :: AVAR | eff ) m
  => TA.CompareToString item
  => Eq item
  => Show err
  => Array item
  -> TA.TypeaheadInput o item source err ( dom :: DOM, avar :: AVAR | eff ) m
defaultMulti' xs =
  { items: TA.Sync xs
  , debounceTime: Milliseconds 0.0
  , search: Nothing
  , initialSelection: TA.Many []
  , render: renderTA defaultRenderItem
  , config: defaultConfig
  }


-- A default multi-select using the default render item function
testAsyncMulti' :: ∀ o item source err eff m
  . MonadAff _ m
 => TA.CompareToString item
 => Eq item
 => Show err
 => source
 -> TA.TypeaheadInput o item source err _ m
testAsyncMulti' source =
  { items: TA.ContinuousAsync "" source NotAsked
  , debounceTime: Milliseconds 500.0
  , search: Nothing
  , initialSelection: TA.Many []
  , render: renderTA defaultRenderItem
  , config: defaultConfig
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

----------
-- Render functions

renderTA :: ∀ o item source err eff m
  . MonadAff ( dom :: DOM, avar :: AVAR | eff ) m
 => TA.CompareToString item
 => Eq item
 => Show err
 => (String -> (Maybe Int) -> Int -> item -> H.HTML Void (C.ContainerQuery o item))
 -> TA.TypeaheadState item source err
 -> H.ParentHTML (TA.TypeaheadQuery o item source err (dom :: DOM, avar :: AVAR | eff) m) (TA.ChildQuery o item (dom :: DOM, avar :: AVAR | eff) ) TA.ChildSlot m
renderTA renderItem st =
	HH.div
	[ HP.class_ $ HH.ClassName "w-full px-3" ]
	[ HH.label
		[ HP.class_ $ HH.ClassName "block uppercase tracking-wide text-grey-darker text-xs font-bold mb-2" ]
		[ HH.text "TA" ]
	, renderSelections
  , HH.slot'
      CP.cp2
      TA.SearchSlot
      S.component
      { render: renderSearch, search: Nothing, debounceTime: Milliseconds 150.0 }
      (HE.input TA.HandleSearch)
  , HH.slot'
      CP.cp1
      TA.ContainerSlot
      C.component
      { render: renderContainer st, items: unpack st.items }
      (HE.input TA.HandleContainer)
	, HH.p
		[ HP.class_ $ HH.ClassName "mt-1 text-grey-dark text-xs" ]
		[ HH.text "This typeahead automatically debounces at 150ms." ]
	]
	where
    unpack (TA.Sync x) = x
    unpack (TA.Async _ x) = withDefault [] x
    unpack (TA.ContinuousAsync _ _ x) = withDefault [] x

    renderSelections
      :: H.ParentHTML
          (TA.TypeaheadQuery o item source err (dom :: DOM, avar :: AVAR | eff) m)
          (TA.ChildQuery o item (dom :: DOM, avar :: AVAR | eff))
          TA.ChildSlot
          m
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

    renderContainer
      :: TA.TypeaheadState item source err
      -> C.ContainerState item
      -> H.HTML Void (C.ContainerQuery o item)
    renderContainer parentState containerState = HH.div [ HP.class_ $ HH.ClassName "relative" ]
      if not containerState.open
        then []
        else [ HH.div
          ( C.getContainerProps
            [ HP.class_ $ HH.ClassName "absolute bg-white shadow h-64 overflow-y-scroll rounded-sm pin-t pin-l w-full" ]
          )
          [ HH.ul
            [ HP.class_ $ HH.ClassName "list-reset" ]
            $ renderItem (parentState.search) containerState.highlightedIndex `mapWithIndex` containerState.items
          ]
        ]

    --  renderSearch
    --    :: S.SearchState _
    --    -> H.HTML Void (S.SearchQuery o item _)
    renderSearch searchState =
                  HH.div
                    [ HP.class_ $ HH.ClassName "flex items-center border-b-2" ]
                    [ HH.input
                      ( S.getInputProps
                        [ HP.class_ $ HH.ClassName "placeholder-grey-dark text-grey-darkest rounded-sm bg-white py-2 px-4 block w-full appearance-none ds-input"
                        , HP.placeholder "Type to search..."
                        , HP.value searchState.search
                        ]
                      )
                    ]


-- A default renderer for items
defaultRenderItem :: ∀ o item
  . TA.CompareToString item
 => Eq item
 => String
 -> (Maybe Int)
 -> Int
 -> item
 -> H.HTML Void (C.ContainerQuery o item)
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

----------
-- Helpers

-- Wrap matching text in a bold span when the user performs a search
boldMatches :: ∀ i p. Pattern -> String -> Array (H.HTML i p)
boldMatches p@(Pattern p') src = html <> lastHtml
  where
    bold = HH.span [ HP.class_ $ HH.ClassName "font-bold" ] [ HH.text p' ]
    alreadySplit = split p src
    html = foldr (\text acc -> [ HH.text text, bold ] <> acc) [] (dropEnd 1 alreadySplit)
    lastHtml = map HH.text $ takeEnd 1 alreadySplit

