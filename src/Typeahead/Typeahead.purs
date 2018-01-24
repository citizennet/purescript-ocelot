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
  , selections :: Array TypeaheadItem }

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
    { initialState: \i -> { items: i.items, selections: [] }
    , render
    , eval
    , receiver: HE.input Receive
    }
  where
    render :: State -> TypeaheadHTML e
    render st =
      HH.div_
      [ renderSelections st
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
    else [ HH.ul_ $ renderSelection <$> st.selections ]
  where
    renderSelection str = HH.li_ [ HH.text str ]

----------
-- Primitive rendering

-- One render function is required per primitive.

renderSearch :: ∀ e. S.SearchState e -> H.HTML Void (SearchQuery e)
renderSearch st = textField "Search Field" "Type to search..." "This typeahead is automatically debounced at 150ms."


renderContainer :: C.ContainerState String -> H.HTML Void ContainerQuery
renderContainer st = HH.div_
  if not st.open
    then []
    else [ renderItems $ renderItem `mapWithIndex` st.items ]
  where
    renderItems :: Array (H.HTML Void ContainerQuery) -> H.HTML Void ContainerQuery
    renderItems html = HH.div ( C.getContainerProps [] ) [ HH.ul_ html ]

    renderItem :: Int -> TypeaheadItem -> H.HTML Void ContainerQuery
    renderItem ix item = HH.li ( C.getItemProps ix [] ) [ HH.text item ]


----------
-- Other render helpers

cssLabel :: ∀ p i. HH.IProp ( "class" ∷ String | p ) i
cssLabel = HP.class_ $ HH.ClassName "f6 b db mb2"
cssInput :: ∀ p i. HH.IProp ( "class" ∷ String | p ) i
cssInput = HP.class_ $ HH.ClassName "input-reset pa2 mb2 db w-100 b--none"
cssHelperText :: ∀ p i. HH.IProp ( "class" ∷ String | p ) i
cssHelperText = HP.class_ $ HH.ClassName "f6 black-60 db mb2"

-- textField :: ∀ e. String -> String -> String -> HTML e
textField :: ∀ p e. String → String → String → HH.HTML p (S.SearchQuery Query TypeaheadItem e Unit)
textField label placeholder helper =
  HH.div
  [ HP.class_ $ HH.ClassName "measure" ]
  [ HH.label
    [ cssLabel ]
    [ HH.text label ]
  , HH.input
    ( S.getInputProps [ cssInput, HP.placeholder placeholder ] )
  , HH.small
    [ cssHelperText ]
    [ HH.text helper ]
  ]
