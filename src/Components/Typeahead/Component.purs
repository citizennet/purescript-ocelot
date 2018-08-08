module Ocelot.Component.Typeahead where

import Prelude

import Control.Alternative (class Plus, empty)
import Data.Array (difference, filter, length, sort, (:))
import Data.Fuzzy (Fuzzy(..))
import Data.Fuzzy as Fuzz
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Rational ((%))
import Data.Time.Duration (Milliseconds)
import Effect.Aff.Class (class MonadAff)
import Foreign.Object (Object)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Network.RemoteData (RemoteData(..))
import Select as Select

----------
-- Components

single :: TypeaheadComponent Maybe
single = base
  { runSelect: const <<< Just
  , runRemove: const (const Nothing)
  , runFilter: \items -> maybe items (\i -> filter (_ == i) items)
  }

multi :: TypeaheadComponent Array
multi = base
  { runSelect: (:)
  , runRemove: filter <<< (/=)
  , runFilter: difference
  }

type TypeaheadComponent f =
  ∀ pq item m
   . MonadAff m
  => Eq item
  => ( State f item m
       -> Select.State (Fuzzy item)
       -> Select.ComponentHTML (Query pq f item) (Fuzzy item)
     )
  -> H.Component HH.HTML (Query pq f item) (Input item m) (Message pq f item) m

----------
-- Component types

type State f item m =
  { items :: RemoteData String (Array item)
  , selections :: f item
  , search :: String
  , config :: Config item m
  , ops :: Operations f item
  }

type Input item m =
  { items :: RemoteData String (Array item)
  , search :: Maybe String
  , config :: Config item m
  }

data Query pq f item a
  = Remove item a
  | RemoveAll a
  | TriggerFocus a
  | Synchronize a
  | Search String a
  | HandleSelect (Select.Message (Query pq f item) (Fuzzy item)) a
  | GetSelections (f item -> a)
  | ReplaceSelections (f item) a
  | ReplaceItems (RemoteData String (Array item)) a
  | Reset a
  | AndThen (Query pq f item Unit) (Query pq f item Unit) a
  | Raise (pq Unit) a

data Message pq f item
  = Searched String
  | SelectionsChanged (f item)
  | Emit (pq Unit)

----------
-- Child types

-- The typeahead relies on the Search and Container primitives.
type ChildSlot = Unit
type ChildQuery pq f item = Select.Query (Query pq f item) (Fuzzy item)

---------
-- Data modeling

type Config item m =
  { insertable :: Insertable item
  , keepOpen   :: Boolean
  , toObject   :: item -> Object String
  , asyncConfig :: Maybe (AsyncConfig item m)
  }

type AsyncConfig item m =
  { debounceTime :: Milliseconds
  , fetchItems   :: String -> m (RemoteData String (Array item))
  }

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
  -> ( State f item m
       -> Select.State (Fuzzy item)
       -> Select.ComponentHTML (Query pq f item) (Fuzzy item)
     )
  -> H.Component HH.HTML (Query pq f item) (Input item m) (Message pq f item) m
base ops renderSelect =
  H.lifecycleParentComponent
    { initialState
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just $ H.action Synchronize
    , finalizer: Nothing
    }
  where
    initialState :: Input item m -> State f item m
    initialState { items, search, config } =
      { items
      , selections: empty :: f item
      , search: fromMaybe "" search
      , config
      , ops
      }

    render
      :: State f item m
      -> H.ParentHTML (Query pq f item) (ChildQuery pq f item) ChildSlot m
    render st =
      HH.slot unit Select.component selectInput (HE.input HandleSelect)

      where

      selectInput =
        { inputType: Select.TextInput
        , items: []
        , initialSearch: Nothing
        , debounceTime: (Just <<< _.debounceTime) =<< st.config.asyncConfig
        , render: renderSelect st
        }

    eval
      :: Query pq f item
      ~> H.ParentDSL
          (State f item m)
          (Query pq f item)
          (ChildQuery pq f item)
          ChildSlot
          (Message pq f item)
          m
    eval = case _ of
      Search text a ->
        eval $ HandleSelect ( Select.Searched text ) a

      HandleSelect message a -> case message of
        Select.Emit query -> eval query $> a

        Select.Selected (Fuzzy { original: item }) -> do
          st <- H.modify \st -> st { selections = st.ops.runSelect item st.selections }
          _ <- if st.config.keepOpen
               then pure Nothing
               else H.query unit $ Select.setVisibility Select.Off
          H.raise $ SelectionsChanged st.selections
          eval $ Synchronize a

        -- Perform a new search, fetching data if Async.
        Select.Searched text -> do
          st <- H.get
          H.modify_ _ { search = text }

          case st.config.asyncConfig of
            Nothing -> pure unit
            Just { fetchItems } -> do
              H.modify_ _ { items = Loading }
              _ <- eval $ Synchronize a
              newItems <- H.lift $ fetchItems text
              H.modify_ _ { items = newItems }

          H.raise $ Searched text
          eval $ Synchronize a

        Select.VisibilityChanged _ -> pure a

      -- Remove a currently-selected item.
      Remove item a -> do
        st <- H.modify \st -> st { selections = st.ops.runRemove item st.selections }
        H.raise $ SelectionsChanged st.selections
        eval $ Synchronize a

      -- Remove all the items.
      RemoveAll a -> do
        st <- H.modify \st -> st { selections = empty :: f item }
        H.raise $ SelectionsChanged st.selections
        eval $ Synchronize a

      -- Tell the Select to trigger focus on the input
      TriggerFocus a -> a <$ do
        H.query unit Select.triggerFocus

      -- Tell the parent what the current state of the Selections list is.
      GetSelections reply -> do
        { selections } <- H.get
        pure $ reply selections

      -- Update the state of Select to be in sync.
      Synchronize a -> do
        st <- H.get

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
        H.modify_ _ { items = items }
        eval $ Synchronize a

      ReplaceSelections selections a -> do
        H.modify_ _ { selections = selections }
        eval $ Synchronize a

      Reset a -> do
        st <- H.modify _ { selections = empty :: f item, items = NotAsked }
        H.raise $ SelectionsChanged st.selections
        eval $ Synchronize a

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
  <$> (map (flip st.ops.runFilter st.selections) st.items)
  where
    matcher :: item -> Fuzzy item
    matcher = Fuzz.match true st.config.toObject st.search

    fuzzyItems :: Array item -> Array (Fuzzy item)
    fuzzyItems = map matcher

    applyI :: Array (Fuzzy item) -> Array (Fuzzy item)
    applyI = applyInsertable matcher st.config.insertable st.search

    applyF :: Array (Fuzzy item) -> Array (Fuzzy item)
    applyF = filter (\(Fuzzy { ratio }) -> ratio > (2 % 3))

applyInsertable
  :: ∀ item
   . (item -> Fuzzy item)
  -> Insertable item
  -> String
  -> Array (Fuzzy item)
  -> Array (Fuzzy item)
applyInsertable match insertable text items = case insertable of
  NotInsertable -> items
  Insertable mkItem | length (filter isExactMatch items) > 0 -> items
                    | otherwise -> (match $ mkItem text) : items
    where
      isExactMatch (Fuzzy { distance }) = distance == Fuzz.Distance 0 0 0 0 0 0
