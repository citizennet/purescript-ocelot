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
import Select.Primitive.Container as C
import Select.Primitive.Search as S
import CN.UI.Core.Typeahead as Typeahead


----------
-- Default typeahead

defaultMulti :: ∀ o item e. Typeahead.StringComparable item => Array item -> Typeahead.TypeaheadInput o item e
defaultMulti xs =
  { items: xs
  , debounceTime: Milliseconds 0.0
  , search: Nothing
  , initialSelection: Typeahead.Many []
  , render: renderTypeahead
  , config: Right Typeahead.defaultConfig
  }


----------
-- Render functions

renderTypeahead :: ∀ o item e. Typeahead.StringComparable item
	=> Typeahead.TypeaheadState o item e
	-> Typeahead.TypeaheadHTML o item e
renderTypeahead st@(Typeahead.State st') =
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
		renderContainer (Typeahead.State parentSt) st = HH.div [ HP.class_ $ HH.ClassName "relative" ]
			if not st.open
				then []
				else [ HH.div
					( C.getContainerProps
						[ HP.class_ $ HH.ClassName "absolute bg-white shadow rounded-sm pin-t pin-l w-full" ]
					)
					[ HH.ul
						[ HP.class_ $ HH.ClassName "list-reset" ]
						$ renderItem (parentSt.search) `mapWithIndex` st.items
					]
				]
			where
				renderItem :: String -> Int -> item -> H.HTML Void (C.ContainerQuery o item)
				renderItem "" ix item = HH.li
					( C.getItemProps ix
						[ HP.class_ $ HH.ClassName $ "px-4 py-1 text-grey-darkest" <> hover ]
					)
						[ HH.text $ Typeahead.toString item ]
					where
						hover = if st.highlightedIndex == Just ix then " bg-grey-lighter" else ""

				renderItem search ix item = HH.li
					( C.getItemProps ix
						[ HP.class_ $ HH.ClassName $ "px-4 py-1 text-grey-darkest" <> hover ]
					)
					( boldMatches (Pattern search) $ Typeahead.toString item )
					where
						hover = if st.highlightedIndex == Just ix then " bg-grey-lighter" else ""

		renderSearch
			:: S.SearchState e
			-> H.HTML Void (S.SearchQuery o item e)
		renderSearch _ =
			HH.input
			( S.getInputProps
				[ HP.class_ $ HH.ClassName "placeholder-grey-dark text-grey-darkest rounded-sm bg-white py-2 px-4 block w-full appearance-none ds-input"
				, HP.placeholder "Type to search..."
				]
			)


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

