module CN.UI.Core.Typeahead2 where

import Prelude

import Network.RemoteData (RemoteData(..))
import Control.Monad.Aff.Console (logShow)
import Control.Monad.Aff.Class (class MonadAff)

import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), contains, toLower)
import Data.Tuple (Tuple(..))
import Data.Array ((:), filter, length)
import Data.Either.Nested (Either2)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.Time.Duration (Milliseconds)

import Control.Comonad (extract)
import Control.Comonad.Store (Store, store, seeks)

import Halogen as H
import Halogen.HTML as HH
import Halogen.Component.ChildPath as CP

import Select.Effects (Effects)
import Select.Primitives.Container as C
import Select.Primitives.Search as S
import Select.Primitives.State (getState)


----------
-- Component types

type State o item source err eff m =
  Store
    (TypeaheadState item source err)
    (H.ParentHTML (TypeaheadQuery o item source err eff m) (ChildQuery o item eff) ChildSlot m)

type TypeaheadState item source err =
  { items :: SyncMethod source err (Array item)
  , selections :: SelectionType item
  , debounceTime :: Milliseconds
  , search :: String
  , config :: Config item
  }

type TypeaheadInput o item source err eff m =
  { items :: SyncMethod source err (Array item)
  , debounceTime :: Milliseconds
  , search :: Maybe String
  , initialSelection :: SelectionType item
  , render
    :: TypeaheadState item source err
    -> H.ParentHTML (TypeaheadQuery o item source err eff m) (ChildQuery o item eff) ChildSlot m
  , config :: Config item
  }

data TypeaheadQuery o item source err eff m a
  = HandleContainer (C.Message o item) a
  | HandleSearch (S.Message o item) a
  | Remove item a
  | Selections (SelectionType item -> a)
  | FulfillRequest (SyncMethod source err (Array item)) a
  | TypeaheadReceiver (TypeaheadInput o item source err eff m) a

data TypeaheadMessage o item source err
  = ItemSelected item
  | ItemRemoved item
  | NewSearch String
  | RequestData (SyncMethod source err (Array item))
  | Emit (o Unit)


----------
-- Child types

type ChildQuery o item eff = Coproduct2
  (C.ContainerQuery o item)
  (S.SearchQuery    o item (Effects eff))
type ChildSlot = Either2 Slot Slot

data Slot
  = ContainerSlot
  | SearchSlot
derive instance eqPrimitiveSlot :: Eq Slot
derive instance ordPrimitiveSlot :: Ord Slot


----------
-- Data modeling

type Config item =
  { filterType  :: FilterType item
  , insertable  :: Insertable item
  , keepOpen    :: Boolean
  }

data FilterType item
  = NoFilter
  | Exact
  | CaseInsensitive
  | CustomMatch (String -> item -> Boolean)

data Insertable item
  = NotInsertable
  | Insertable (String -> item)

data SyncMethod source err a
  = Sync a
  | Async source (RemoteData err a)
  | ContinuousAsync String source (RemoteData err a)
derive instance functorSyncMethod :: Functor (SyncMethod source err)

data SelectionType item
  = One (Maybe item)
  | Many (Array item)
derive instance functorSelectionType :: Functor SelectionType


class CompareToString a where
  comparableStr :: a -> String

instance compareToStringString :: CompareToString String where
  comparableStr = id


----------
-- Component

component :: ∀ o item source err eff m
  . MonadAff (Effects eff) m
 => CompareToString item
 => Eq item
 => Show err
 => H.Component
      HH.HTML
      (TypeaheadQuery o item source err eff m)
      (TypeaheadInput o item source err eff m)
      (TypeaheadMessage o item source err)
      m
component =
  H.parentComponent
    { initialState
    , render: extract
    , eval
    , receiver: const Nothing
    }
  where
    initialState
      :: (TypeaheadInput o item source err eff m)
      -> State o item source err eff m
    initialState i = store i.render
      { items: i.items
      , selections: i.initialSelection
      , debounceTime: i.debounceTime
      , search: fromMaybe "" i.search
      , config: i.config
      }

    eval
      :: (TypeaheadQuery o item source err eff m)
      ~> H.ParentDSL
          (State o item source err eff m)
          (TypeaheadQuery o item source err eff m)
          (ChildQuery o item eff)
          (ChildSlot)
          (TypeaheadMessage o item source err)
          m
    eval = case _ of
      HandleContainer message a -> case message of
        C.Emit query -> do
           H.raise (Emit query)
           pure a

        C.ItemSelected item -> do
          (Tuple _ st) <- getState
          let (Tuple items' selections') = selectItem item st.items st.selections
          H.modify $ seeks _ { items = items', selections = selections' }
          _ <- updateContainer items'
          _ <- H.query' CP.cp2 SearchSlot $ H.action $ S.TextInput ""
          _ <- if st.config.keepOpen
               then pure Nothing
               else H.query' CP.cp1 ContainerSlot $ H.action $ C.Visibility C.Off
          H.raise $ ItemSelected item
          pure a

      HandleSearch message a -> case message of
        S.Emit query -> H.raise (Emit query) *> pure a
        S.ContainerQuery query -> H.query' CP.cp1 ContainerSlot query *> pure a

        S.NewSearch text -> do
          H.modify $ seeks _ { search = text }

          (Tuple _ st) <- getState

          let applyI = applyInsertable st.config.insertable text
              applyF = applyFilter st.config.filterType text

          items' <- case st.items of
            Sync i -> pure $ Sync $ (applyI <<< applyF) i
            Async src i -> pure $ Async src $ (applyI <<< applyF) <$> i
            ContinuousAsync _ src _ -> do
              let cont = ContinuousAsync text src Loading
              H.modify $ seeks $ _ { items = cont }
              H.raise $ RequestData cont
              pure cont

          _ <- updateContainer items'

          H.raise $ NewSearch text
          pure a

      Remove item a -> do
        (Tuple _ st) <- getState
        let (Tuple items' selections') = removeItem item st.items st.selections
        H.modify $ seeks _ { selections = selections', items = items' }
        _ <- updateContainer items'
        H.raise $ ItemRemoved item
        pure a

      Selections reply -> do
        (Tuple _ st) <- getState
        pure $ reply st.selections

      FulfillRequest items a -> do
        H.modify $ seeks $ _ { items = items }
        _ <- updateContainer items
        pure a

      TypeaheadReceiver input a -> do
        H.put (initialState input)
        pure a

    updateContainer (Sync i) = updateContainerWith (Success i)
    updateContainer (Async _ i) = updateContainerWith i
    updateContainer (ContinuousAsync _ _ i) = updateContainerWith i

    updateContainerWith (Failure e) = do
      _ <- H.query' CP.cp1 ContainerSlot
         $ H.action
         $ C.Visibility C.Off
      _ <- H.query' CP.cp1 ContainerSlot
         $ H.action
         $ C.ReplaceItems []
      H.liftAff $ logShow e
    updateContainerWith (Success items') = do
      _ <- H.query' CP.cp1 ContainerSlot
         $ H.action
         $ C.ReplaceItems items'
      pure unit
    updateContainerWith _ = pure unit



----------
-- Helpers

applyFilter :: ∀ item. CompareToString item => FilterType item -> String -> Array item -> Array item
applyFilter filterType text items = case filterType of
  NoFilter -> items
  Exact -> filter (\item -> contains (Pattern text) (comparableStr item)) items
  CaseInsensitive ->
    filter (\item -> contains (Pattern $ toLower text) (toLower $ comparableStr item)) items
  CustomMatch match -> filter (\item -> match text item) items

applyInsertable :: ∀ item. CompareToString item => Insertable item -> String -> Array item -> Array item
applyInsertable insertable text items = case insertable of
  NotInsertable -> items
  Insertable mkItem | length items > 0 -> items
                    | otherwise -> (mkItem text) : items

removeItem :: ∀ item source err
  . CompareToString item
 => Eq item
 => item
 -> SyncMethod source err (Array item)
 -> SelectionType item
 -> Tuple (SyncMethod source err (Array item)) (SelectionType item)
removeItem item items selections = case selections of
  One  _  -> Tuple (insert item items) (One Nothing)
  Many xs -> Tuple (insert item items) (Many $ filter ((/=) item) xs)
    where
      insert i is = (\i' -> i : i') <$> is

selectItem :: ∀ item source err
  . CompareToString item
 => Eq item
 => item
 -> SyncMethod source err (Array item)
 -> SelectionType item
 -> Tuple (SyncMethod source err (Array item)) (SelectionType item)
selectItem item items selections = case selections of
  One Nothing  -> Tuple (remove item items) (One $ Just item)
  One (Just i) -> Tuple (insert i $ remove item items) (One $ Just item)
  Many xs      -> Tuple (remove item items) (Many $ item : xs)
    where
      insert i is = (\i' -> i : i') <$> is
      remove i is = (filter ((/=) i)) <$> is

