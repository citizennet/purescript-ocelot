module CN.UI.Typeahead where

import Prelude
import Data.Array ((:), length, mapWithIndex, filter, difference)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), contains)
import Data.Time.Duration (Milliseconds(..))
import Select.Dispatch
import Select.Effects (FX)
import Select.Primitive.Container as C
import Select.Primitive.Search as S
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

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
data Query e a
  = HandleContainer (C.Message String (Query e) e) a
  | HandleSearch (S.Message String (Query e) e) a
  | Receive TypeaheadInput a

-- Component top level definition
type TypeaheadComponent e
  = H.Component HH.HTML (Query e) TypeaheadInput TypeaheadMessage (FX e)

-- Component input type
type TypeaheadInput =
  { items :: Array TypeaheadItem }

-- Component message type
type TypeaheadMessage = Void

-- Component child types
type ChildQuery e = Dispatch TypeaheadItem (Query e) e
type ChildSlot = Slot

-- Primitive slot types
data PrimitiveSlot
  = ContainerSlot
  | SearchSlot

derive instance eqPrimitiveSlot :: Eq PrimitiveSlot
derive instance ordPrimitiveSlot :: Ord PrimitiveSlot

-- Enclosing slot type
data Slot = Slot PrimitiveSlot

derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot


-- Return from render function
type TypeaheadHTML e =
  H.ParentHTML (Query e) (ChildQuery e) ChildSlot (FX e)

-- Return from eval function
type TypeaheadDSL e =
  H.ParentDSL State (Query e) (ChildQuery e) ChildSlot TypeaheadMessage (FX e)


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
      [ HH.slot
          ( Slot SearchSlot )
          S.component
          { render: renderSearch, search: Nothing, debounceTime: Milliseconds 150.0 }
          ( HE.input HandleSearch )
      , HH.slot
          ( Slot ContainerSlot )
          C.component
          { render: renderContainer, items: st.items }
          ( HE.input HandleContainer )
      ]

    eval :: Query e ~> TypeaheadDSL e
    eval = case _ of
      HandleSearch message a -> case message of
        S.Emit query -> case query of
          Container containerQuery _ -> do
            _ <- H.query (Slot ContainerSlot)
                  $ H.action
                  $ Container containerQuery
            pure a

          ParentQuery parentQuery _ -> a <$ eval parentQuery

          Search _ _ -> pure a

        S.NewSearch text -> a <$ do
          st <- H.get
          let matches i = filter (\i' -> contains (Pattern i) i')
              available = difference (matches text st.items) st.selections

              -- Allow searches with no matches to be inserted
              newItems
                | length available < 1 = text : available
                | otherwise            = available

          -- Send the new items to the container
          _ <- H.query (Slot ContainerSlot)
                $ H.action
                $ Container
                $ ContainerReceiver { render: renderContainer, items: newItems }

          pure a

      HandleContainer message a -> case message of
        -- we don't embed any other queries except parent queries
        C.Emit query -> emit eval query a

        C.ItemSelected item -> a <$ do
          st <- H.get
          if length (filter ((==) item) st.items) > 0
            then H.modify _ { selections = item : st.selections }
            else H.modify _ { items = item : st.items, selections = item : st.selections }

          newState <- H.get
          let newItems = difference newState.items newState.selections

          _ <- H.query (Slot ContainerSlot)
                $ H.action
                $ Container
                $ ContainerReceiver { render: renderContainer, items: newItems }

          pure a

      Receive { items } a -> H.modify _ { items = items } *> pure a


----------
-- Render helpers

-- One render function is required per primitive.

renderSearch :: ∀ e. SearchState e -> H.HTML Void (ChildQuery e)
renderSearch st = HH.input ( getInputProps [] )

renderContainer :: ∀ e. ContainerState String -> H.HTML Void (ChildQuery e)
renderContainer st = HH.div_
  if not st.open
    then []
    else [ renderItems $ renderItem `mapWithIndex` st.items ]
  where
    renderItems :: Array (H.HTML Void (ChildQuery e)) -> H.HTML Void (ChildQuery e)
    renderItems html = HH.div ( getContainerProps [] ) [ HH.ul_ html ]

    renderItem :: Int -> TypeaheadItem -> H.HTML Void (ChildQuery e)
    renderItem ix item = HH.li ( getItemProps ix [] ) [ HH.text item ]
