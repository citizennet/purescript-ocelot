module CN.UI.Typeahead where

import Prelude

import Data.Array ((:), length, mapWithIndex, filter, difference)
import Data.Either.Nested (Either2)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), contains)
import Data.Time.Duration (Milliseconds(..))
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Select.Effects (FX)
import Select.Primitive.Container as C
import Select.Primitive.Search as S

----------
-- Item Types

type TypeaheadItem = String


----------
-- Component Types

-- Component state definition
type State =
  { items      :: Array TypeaheadItem
  , selections :: Array TypeaheadItem
  , search     :: Maybe String }

-- Component query definition
data Query a
  = HandleContainer (C.Message Query String) a
  | HandleSearch (S.Message Query String) a
  | Receive TypeaheadInput a

-- Component top level definition
type TypeaheadComponent e
  = H.Component HH.HTML Query TypeaheadInput TypeaheadMessage (FX e)

-- Component input type
type TypeaheadInput =
  { items :: Array TypeaheadItem }

-- Component message type
type TypeaheadMessage = Void

-- Component child types
type ContainerQuery = C.ContainerQuery Query TypeaheadItem
type SearchQuery e = S.SearchQuery Query TypeaheadItem e
type ChildQuery e = Coproduct2 ContainerQuery (SearchQuery e)
type ChildSlot = Either2 Slot Slot

-- Primitive slot types
data PrimitiveSlot
  = ContainerSlot
  | SearchSlot
derive instance eqPrimitiveSlot :: Eq PrimitiveSlot
derive instance ordPrimitiveSlot :: Ord PrimitiveSlot

-- Enclosing slot type
data Slot = Slot PrimitiveSlot
derive instance eqSlot  :: Eq Slot
derive instance ordSlot :: Ord Slot


-- Return from render function
type TypeaheadHTML e =
  H.ParentHTML Query (ChildQuery e) ChildSlot (FX e)

-- Return from eval function
type TypeaheadDSL e =
  H.ParentDSL State Query (ChildQuery e) ChildSlot TypeaheadMessage (FX e)


----------
-- Component definition

component :: ∀ e. TypeaheadComponent e
component =
  H.parentComponent
    { initialState: \i -> { items: i.items, search: Nothing,  selections: [] }
    , render
    , eval
    , receiver: HE.input Receive
    }
  where
    render :: State -> TypeaheadHTML e
    render st =
      HH.div
      [ HP.class_ $ HH.ClassName "w-full px-3" ]
      [ HH.label
        [ HP.class_ $ HH.ClassName "block uppercase tracking-wide text-grey-darker text-xs font-bold mb-2" ]
        [ HH.text "Typeahead" ]
      , renderSelections st
      , HH.slot'
          CP.cp2
          ( Slot SearchSlot )
          S.component
          { render: renderSearch, search: Nothing, debounceTime: Milliseconds 150.0 }
          ( HE.input HandleSearch )
      , HH.slot'
          CP.cp1
          ( Slot ContainerSlot )
          C.component
          { render: renderContainer, items: st.items }
          ( HE.input HandleContainer )
      , HH.p
        [ HP.class_ $ HH.ClassName "mt-1 text-grey-dark text-xs" ]
        [ HH.text "This typeahead automatically debounces at 150ms." ]
      ]

    eval :: Query ~> TypeaheadDSL e
    eval = case _ of
      HandleSearch message a -> case message of
        S.ContainerQuery query -> do
          _ <- H.query' CP.cp1 (Slot ContainerSlot) query
          pure a

        S.NewSearch text -> do
          st <- H.get
          let matches i = filter (\i' -> contains (Pattern i) i')
              available = difference (matches text st.items) st.selections

              -- Allow searches with no matches to be inserted
              newItems
                | length available < 1 = text : available
                | otherwise            = available

          -- Send the new items to the container
          _ <- H.query' CP.cp1 (Slot ContainerSlot)
                $ H.action
                $ C.ContainerReceiver { render: renderContainer, items: newItems }

          pure a

        S.Emit query -> eval query *> pure a

      HandleContainer message a -> case message of
        -- we don't embed any other queries except parent queries
        C.Emit query -> eval query *> pure a

        C.ItemSelected item -> a <$ do
          st <- H.get
          if length (filter ((==) item) st.items) > 0
            then H.modify _ { selections = item : st.selections }
            else H.modify _ { items = item : st.items, selections = item : st.selections }

          newState <- H.get
          let newItems = difference newState.items newState.selections

          _ <- H.query' CP.cp1 (Slot ContainerSlot)
                $ H.action
                $ C.ContainerReceiver { render: renderContainer, items: newItems }

          _ <- H.query' CP.cp1 (Slot ContainerSlot)
                $ H.action
                $ C.Visibility C.Off

          pure a

      Receive { items } a -> H.modify _ { items = items } *> pure a


----------
-- Render helpers

renderSelections :: ∀ e. State -> TypeaheadHTML e
renderSelections st = HH.div_
  if length st.selections <= 0
    then []
    else [ HH.div
      [ HP.class_ $ HH.ClassName "bg-white rounded-sm w-full border-b border-grey-lighter" ]
      [ HH.ul
        [ HP.class_ $ HH.ClassName "list-reset" ]
        $ renderSelection <$> st.selections ]
      ]
  where
    renderSelection str = HH.li
      [ HP.class_ $ HH.ClassName "px-4 py-1 text-grey-darkest" ]
      [ HH.text str ]

renderContainer :: C.ContainerState String -> H.HTML Void ContainerQuery
renderContainer st = HH.div [ HP.class_ $ HH.ClassName "relative" ]
  if not st.open
    then []
    else [ HH.div
      ( C.getContainerProps
        [ HP.class_ $ HH.ClassName "absolute bg-white shadow rounded-sm pin-t pin-l w-full" ]
      )
      [ HH.ul
        [ HP.class_ $ HH.ClassName "list-reset" ]
        $ renderItem `mapWithIndex` st.items
      ]
    ]
  where
    renderItem :: Int -> TypeaheadItem -> H.HTML Void ContainerQuery
    renderItem ix item = HH.li
      ( C.getItemProps ix
        [ HP.class_ $ HH.ClassName $ "px-4 py-1 text-grey-darkest" <> hover ]
      )
      [ HH.text item ]
      where
        hover = if st.highlightedIndex == Just ix then " bg-grey-lighter" else ""

renderSearch :: ∀ e. S.SearchState e -> H.HTML Void (SearchQuery e)
renderSearch _ =
  HH.input
  ( S.getInputProps
    [ HP.class_ $ HH.ClassName "placeholder-grey-dark text-grey-darkest rounded-sm bg-white py-2 px-4 block w-full appearance-none ds-input"
    , HP.placeholder "Type to search..."
    ]
  )
