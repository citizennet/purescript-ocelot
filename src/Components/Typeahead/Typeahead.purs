module CN.UI.Components.Typeahead where

import Prelude

import Data.Array (dropEnd, mapWithIndex, takeEnd)
import Data.Maybe (Maybe(Just, Nothing))
import Data.String (Pattern(Pattern), split)
import Data.Time.Duration (Milliseconds(..))
import Data.Either (Either(..))
import Data.Foldable (foldr)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Select.Primitives.Container as C
import Select.Primitives.Search as S
import CN.UI.Core.Typeahead as Typeahead


----------
-- Default typeahead

-- A default multi-select that is provided with a renderItem function to determine
-- rendering a specific item in the container
defaultMulti :: ∀ o item e
   . Typeahead.StringComparable item
  => Eq item
  => Array item
  -> (String -> (Maybe Int) -> Int -> item -> H.HTML Void (C.ContainerQuery o item))
  -> Typeahead.TypeaheadInput o item e
defaultMulti xs renderItem =
  { items: xs
  , debounceTime: Milliseconds 0.0
  , search: Nothing
  , initialSelection: Typeahead.Many []
  , render: renderTypeahead renderItem
  , config: Right Typeahead.defaultConfig
  }

-- A default multi-select using the default render item function
defaultMulti' :: ∀ o item e
   . Typeahead.StringComparable item
  => Eq item
  => Array item
  -> Typeahead.TypeaheadInput o item e
defaultMulti' xs =
  { items: xs
  , debounceTime: Milliseconds 0.0
  , search: Nothing
  , initialSelection: Typeahead.Many []
  , render: renderTypeahead defaultRenderItem
  , config: Right Typeahead.defaultConfig
  }


----------
-- Render functions

renderTypeahead :: ∀ o item e. Typeahead.StringComparable item
  => (String -> (Maybe Int) -> Int -> item -> H.HTML Void (C.ContainerQuery o item))
	-> Typeahead.TypeaheadState o item e
	-> Typeahead.TypeaheadHTML o item e
renderTypeahead renderItem st@(Typeahead.State st') =
	HH.div
	[ HP.class_ $ HH.ClassName "w-full px-3" ]
	[ HH.label
		[ HP.class_ $ HH.ClassName "block uppercase tracking-wide text-grey-darker text-xs font-bold mb-2" ]
		[ HH.text "Typeahead" ]
	, renderSelections
	, Typeahead.searchSlot
			{ render: renderSearch, search: Nothing, debounceTime: Milliseconds 150.0 }
	, Typeahead.containerSlot
			{ render: renderContainer st, items: st'.items }
	, HH.p
		[ HP.class_ $ HH.ClassName "mt-1 text-grey-dark text-xs" ]
		[ HH.text "This typeahead automatically debounces at 150ms." ]
	]
	where
		renderSelections :: Typeahead.TypeaheadHTML o item e
		renderSelections =
			case st'.selections of
				(Typeahead.One Nothing) -> HH.div_ []

				(Typeahead.One (Just x)) -> HH.div_
					[ HH.div
						[ HP.class_ $ HH.ClassName "bg-white rounded-sm w-full text-grey-darkest border-b border-grey-lighter" ]
						[ HH.ul
							[ HP.class_ $ HH.ClassName "list-reset" ]
							[ renderSelection x ]
						]
					]

				(Typeahead.Many xs) -> HH.div_
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
						[ HH.text $ Typeahead.toString item ]
					, HH.span
						[ HP.class_ $ HH.ClassName "absolute pin-t pin-b pin-r p-1 mx-3 cursor-pointer"
						, HE.onClick (HE.input_ (Typeahead.Remove item)) ]
						[ HH.text "×" ]
					]

		renderContainer
			:: Typeahead.TypeaheadState o item e
			-> C.ContainerState item
			-> H.HTML Void (C.ContainerQuery o item)
		renderContainer (Typeahead.State parentState) containerState = HH.div [ HP.class_ $ HH.ClassName "relative" ]
			if not containerState.open
				then []
				else [ HH.div
					( C.getContainerProps
						[ HP.class_ $ HH.ClassName "absolute bg-white shadow rounded-sm pin-t pin-l w-full" ]
					)
					[ HH.ul
						[ HP.class_ $ HH.ClassName "list-reset" ]
						$ renderItem (parentState.search) containerState.highlightedIndex `mapWithIndex` containerState.items
					]
				]

		renderSearch
			:: S.SearchState e
			-> H.HTML Void (S.SearchQuery o item e)
		renderSearch searchState =
			HH.input
			( S.getInputProps
				[ HP.class_ $ HH.ClassName "placeholder-grey-dark text-grey-darkest rounded-sm bg-white py-2 px-4 block w-full appearance-none ds-input"
				, HP.placeholder "Type to search..."
        , HP.value searchState.search
				]
			)


-- A default renderer for items
defaultRenderItem :: ∀ o item
  . Typeahead.StringComparable item
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
    [ HH.text $ Typeahead.toString item ]
  where
    hover = if highlightIndex == Just itemIndex then " bg-grey-lighter" else ""
defaultRenderItem search highlightIndex itemIndex item = HH.li
  ( C.getItemProps itemIndex
    [ HP.class_ $ HH.ClassName $ "px-4 py-1 text-grey-darkest" <> hover ]
  )
  ( boldMatches (Pattern search) $ Typeahead.toString item )
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

