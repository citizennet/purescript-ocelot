module Ocelot.Component.Typeahead.Base where

import Prelude

import Control.Alternative (class Plus, empty)
import Control.Comonad (extract)
import Control.Comonad.Store (Store, store)
import Data.Array (difference, filter, length, sort, (:))
import Data.Fuzzy (Fuzzy(..))
import Data.Fuzzy as Fuzz
import Data.Maybe (Maybe(..), maybe)
import Data.Rational ((%))
import Data.Time.Duration (Milliseconds)
import Effect.Aff.Class (class MonadAff)
import Foreign.Object (Object)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Network.RemoteData (RemoteData(..))
import Renderless.State (getState, modifyState, modifyState_, modifyStore_)
import Select as Select

----------
-- Components

single :: ∀ pq item m. MonadAff m => Eq item => Component pq Maybe item m
single = base
  { runSelect: const <<< Just
  , runRemove: const (const Nothing)
  , runFilter: \items -> maybe items (\i -> filter (_ /= i) items)
  }

multi :: ∀ pq item m. MonadAff m => Eq item => Component pq Array item m
multi = base
  { runSelect: (:)
  , runRemove: filter <<< (/=)
  , runFilter: difference
  }


----------
-- Component types

type Component pq f item m =
  H.Component HH.HTML (Query pq f item m) (Input pq f item m) (Message pq f item) m

type HTML pq f item m =
  H.ParentHTML (Query pq f item m) (ChildQuery pq f item m) ChildSlot m

type DSL pq f item m =
  H.ParentDSL
    (StateStore pq f item m)
    (Query pq f item m)
    (ChildQuery pq f item m)
    ChildSlot
    (Message pq f item)
    m

type StateStore pq f item m = Store
  (State f item m)
  (H.ParentHTML (Query pq f item m) (ChildQuery pq f item m) ChildSlot m)

type State f item m =
  { items :: RemoteData String (Array item)
  , selected :: f item
  , search :: String
  , insertable :: Insertable item
  , keepOpen :: Boolean
  , itemToObject :: item -> Object String
  , ops :: Operations f item
  , debounceTime :: Maybe Milliseconds
  , async :: Maybe (String -> m (RemoteData String (Array item)))
  }

type Input pq f item m =
  { items :: RemoteData String (Array item)
  , insertable :: Insertable item
  , keepOpen :: Boolean
  , itemToObject :: item -> Object String
  , debounceTime :: Maybe Milliseconds
  , async :: Maybe (String -> m (RemoteData String (Array item)))
  , render :: -- All rendering will happen in Select, but with access to parent state
      State f item m
      -> Select.State (Fuzzy item)
      -> Select.ComponentHTML (Query pq f item m) (Fuzzy item)
  }

data Query pq f item m a
  = Remove item a
  | RemoveAll a
  | TriggerFocus a
  | Synchronize a
  | Search String a
  | HandleSelect (Select.Message (Query pq f item m) (Fuzzy item)) a
  | GetSelected (f item -> a)
  | ReplaceSelected (f item) a
  | ReplaceSelectedBy (Array item -> f item) a
  | ReplaceItems (RemoteData String (Array item)) a
  | Reset a
  | AndThen (Query pq f item m Unit) (Query pq f item m Unit) a
  | Receive (Input pq f item m) a
  | Raise (pq Unit) a

data Message pq f item
  = Searched String
  | Selected item
  | SelectionChanged SelectionCause (f item)
  | Emit (pq Unit)

data SelectionCause
  = RemovalQuery
  | ReplacementQuery
  | ResetQuery
  | SelectionMessage
derive instance eqSelectionCause :: Eq SelectionCause

----------
-- Child types

-- The typeahead relies on the Search and Container primitives.
type ChildSlot = Unit
type ChildQuery pq f item m = Select.Query (Query pq f item m) (Fuzzy item)

---------
-- Data modeling

type Operations f item =
  { runSelect  :: item -> f item -> f item
  , runRemove  :: item -> f item -> f item
  , runFilter  :: Array item -> f item -> Array item
  }

data Insertable item
  = NotInsertable
  | Insertable (String -> item)

----------
-- Component

base
  :: ∀ pq f item m
   . MonadAff m
  => Eq item
  => Plus f
  => Operations f item
  -> Component pq f item m
base ops =
  H.lifecycleParentComponent
    { initialState
    , render: extract
    , eval
    , receiver: const Nothing
    , initializer: Just $ H.action Synchronize
    , finalizer: Nothing
    }
  where
    initialState :: Input pq f item m -> StateStore pq f item m
    initialState i = store (render' i.render) st
      where
        st =
          { items: i.items
          , selected: empty :: f item
          , search: ""
          , itemToObject: i.itemToObject
          , insertable: i.insertable
          , keepOpen: i.keepOpen
          , debounceTime: i.debounceTime
          , async: i.async
          , ops
          }

    render'
      :: ( State f item m
         -> Select.State (Fuzzy item)
         -> Select.ComponentHTML (Query pq f item m) (Fuzzy item)
         )
      -> State f item m
      -> HTML pq f item m
    render' renderSelect st =
      HH.slot unit Select.component selectInput (HE.input HandleSelect)

      where

      selectInput =
        { inputType: Select.TextInput
        , items: []
        , initialSearch: Nothing
        , debounceTime: st.debounceTime
        , render: renderSelect st
        }

    eval :: Query pq f item m ~> DSL pq f item m
    eval = case _ of
      Search text a ->
        eval $ HandleSelect ( Select.Searched text ) a

      HandleSelect message a -> case message of
        Select.Emit query -> eval query $> a

        Select.Selected (Fuzzy { original: item }) -> do
          st <- modifyState \st -> st { selected = st.ops.runSelect item st.selected }
          _ <- if st.keepOpen
               then pure Nothing
               else H.query unit $ Select.setVisibility Select.Off
          H.raise $ SelectionChanged SelectionMessage st.selected
          H.raise $ Selected item
          eval $ Synchronize a

        -- Perform a new search, fetching data if Async.
        Select.Searched text -> do
          st <- getState
          modifyState_ _ { search = text }

          case st.async of
            Nothing -> pure unit
            Just fetchItems -> do
              modifyState_ _ { items = Loading }
              _ <- eval $ Synchronize a
              newItems <- H.lift $ fetchItems text
              modifyState_ _ { items = newItems }

          H.raise $ Searched text
          eval $ Synchronize a

        Select.VisibilityChanged _ -> pure a

      -- Remove a currently-selected item.
      Remove item a -> do
        st <- modifyState \st -> st { selected = st.ops.runRemove item st.selected }
        H.raise $ SelectionChanged RemovalQuery st.selected
        eval $ Synchronize a

      -- Remove all the items.
      RemoveAll a -> do
        st <- modifyState \st -> st { selected = empty :: f item }
        H.raise $ SelectionChanged RemovalQuery st.selected
        eval $ Synchronize a

      -- Tell the Select to trigger focus on the input
      TriggerFocus a -> a <$ do
        H.query unit Select.triggerFocus

      -- Tell the parent what the current state of the Selection list is.
      GetSelected reply -> do
        { selected } <- getState
        pure $ reply selected

      -- Update the state of Select to be in sync.
      Synchronize a -> do
        st <- getState

        _ <- case getNewItems st of
          Success items -> do
            H.query unit $ Select.replaceItems items
          Failure err -> do
            _ <- H.query unit $ Select.setVisibility Select.Off
            H.query unit $ Select.replaceItems []
          NotAsked -> do
            _ <- H.query unit $ Select.setVisibility Select.Off
            H.query unit $ Select.replaceItems []
          Loading -> do
            H.query unit $ Select.replaceItems []

        pure a

      ReplaceItems items a -> do
        modifyState_ _ { items = items }
        eval $ Synchronize a

      ReplaceSelected selected a -> do
        st <- modifyState _ { selected = selected }
        H.raise $ SelectionChanged ReplacementQuery st.selected
        eval $ Synchronize a

      ReplaceSelectedBy f a -> do
        { items } <- getState
        case items of
          Success items' -> eval $ ReplaceSelected (f items') a
          _ -> pure a

      Reset a -> do
        st <- modifyState _ { selected = empty :: f item, items = NotAsked }
        H.raise $ SelectionChanged ResetQuery st.selected
        eval $ Synchronize a

      Receive { render } a -> do
        modifyStore_ (render' render) identity
        pure a

      AndThen q1 q2 a -> eval q1 *> eval q2 $> a

      Raise pq a -> H.raise (Emit pq) $> a


----------
-- Internal helpers

-- Attempt to match new items against the user's search.
getNewItems
  :: ∀ f item m
   . MonadAff m
  => Eq item
  => State f item m
  -> RemoteData String (Array (Fuzzy item))
getNewItems st =
  sort
  <<< applyF
  <<< applyI
  <<< fuzzyItems
  <$> (map (flip st.ops.runFilter st.selected) st.items)
  where
    matcher :: item -> Fuzzy item
    matcher = Fuzz.match true st.itemToObject st.search

    fuzzyItems :: Array item -> Array (Fuzzy item)
    fuzzyItems = map matcher

    applyI :: Array (Fuzzy item) -> Array (Fuzzy item)
    applyI = applyInsertable matcher st.insertable st.search

    applyF :: Array (Fuzzy item) -> Array (Fuzzy item)
    applyF = filter (\(Fuzzy { ratio }) -> ratio > (2 % 3))

applyInsertable
  :: ∀ item
   . (item -> Fuzzy item)
  -> Insertable item
  -> String
  -> Array (Fuzzy item)
  -> Array (Fuzzy item)
applyInsertable _ _ "" items = items
applyInsertable match insertable text items = case insertable of
  NotInsertable -> items
  Insertable mkItem | length (filter isExactMatch items) > 0 -> items
                    | otherwise -> (match $ mkItem text) : items
  where
    isExactMatch (Fuzzy { distance }) = distance == Fuzz.Distance 0 0 0 0 0 0
