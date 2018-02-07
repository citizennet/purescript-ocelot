module CN.UI.Core.Typeahead where

import Prelude

import Network.RemoteData (RemoteData(Success, Failure, Loading))
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Console (logShow, CONSOLE)
import Control.Monad.Aff.Class (class MonadAff)

import DOM (DOM)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(Pattern), contains, toLower)
import Data.Tuple (Tuple(..))
import Data.Array (filter, length, (:))
import Data.Either.Nested (Either2)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.Time.Duration (Milliseconds)

import Control.Comonad (extract)
import Control.Comonad.Store (Store, store, seeks)

import Halogen as H
import Halogen.HTML as HH
import Halogen.Component.ChildPath as CP

import Select.Primitives.Container as C
import Select.Primitives.Search as S
import Select.Primitives.State (getState)


----------
-- Component types

-- The render function is provided outside the component, so we rely
-- on the `Store` type here to make that possible.
type State o item source err eff m =
  Store
    (TypeaheadState item source err)
    (H.ParentHTML (TypeaheadQuery o item source err eff m) (ChildQuery o item eff) ChildSlot m)

-- Items are wrapped in `SyncMethod` to account for failure and loading cases
-- in asynchronous typeaheads. Selections are wrapped in `SelectionType` to
-- account for various limits on what can be selected. The component is responsible
-- for managing its items and selections.
type TypeaheadState item source err =
  { items :: SyncMethod source err (Array item)
  , selections :: SelectionType item
  , search :: String
  , config :: Config item
  }

type TypeaheadInput o item source err eff m =
  { items :: SyncMethod source err (Array item)
  , search :: Maybe String
  , initialSelection :: SelectionType item
  , render
    :: TypeaheadState item source err
    -> H.ParentHTML (TypeaheadQuery o item source err eff m) (ChildQuery o item eff) ChildSlot m
  , config :: Config item
  }

-- `HandleContainer` & `HandleSearch`: Manage routing for child messages
-- `Remove`: The user has removed a currently-selected item.
-- `Selections`: The parent wants to know the current selections.
-- `FulfillRequest`: The parent has fetched data for an async typeahead.
-- `Initialize`: Async typeaheads should fetch their data.
-- `TypeaheadReceiver`: Refresh the typeahead with new input
data TypeaheadQuery o item source err eff m a
  = HandleContainer (C.Message o item) a
  | HandleSearch (S.Message o item) a
  | Remove item a
  | Selections (SelectionType item -> a)
  | FulfillRequest (SyncMethod source err (Array item)) a
  | Initialize a
  | TypeaheadReceiver (TypeaheadInput o item source err eff m) a

-- The parent is notified when items are selected or removed and when a
-- new search is performed, but does not need to take action. This is just
-- for observation purposes. However, `RequestData` represents that a
-- typeahead needs data; the parent is responsible for fetching it and using
-- the `FulfillRequest` method to return the data.
data TypeaheadMessage o item source err
  = ItemSelected item
  | ItemRemoved item
  | NewSearch String
  | RequestData (SyncMethod source err (Array item))
  | Emit (o Unit)


----------
-- Child types

-- The typeahead relies on the Search and Container primitives.
type ChildQuery o item eff = Coproduct2
  (C.ContainerQuery o item)
  (S.SearchQuery    o item eff)
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

-- If an item is meant to be insertable, you must provide a function
-- describing how to move from a search string to an item. Note: this
-- only inserts the item; if you want to take more actions like add to
-- your database, make sure you provide a way to identify 'inserted' items
-- in your custom Item type.
data Insertable item
  = NotInsertable
  | Insertable (String -> item)

-- Sync represents a traditional typeahead. Provide data on input and it will
-- only refresh if you send new input.
--
-- Async behaves like Sync, but fetches its own data on initialization.
--
-- ContinuousAsync fetches data on every user search. Recommended to set a
-- reasonable debounce time on the component.
--
-- `source` is an arbitrary representation for data the parent needs to fetch.
-- Typically this will be a record the parent can use to perform a request.
data SyncMethod source err a
  = Sync a
  | Async source (RemoteData err a)
  | ContinuousAsync Milliseconds String source (RemoteData err a)
derive instance functorSyncMethod :: Functor (SyncMethod source err)

-- A convenience type for when you mount a sync typeahead, filling in the
-- source and error fields with Void on your behalf.
type TypeaheadSyncMessage o item = TypeaheadMessage o item Void Void

-- How many items it is possible to select on the typeahead. NOTE: The limit
-- case will not allow more than N items to be selected, but you can provide
-- an array with more than N items. This will disallow further selections until
-- there are less than N selections.
data SelectionType item
  = One (Maybe item)
  | Limit Int (Array item)
  | Many (Array item)
derive instance functorSelectionType :: Functor SelectionType

-- A type class that guarantees your item type can be compared to a user search
-- string for filtering purposes. You can rely on a default Show instance with:
--
-- ```
-- instance compareToStringX :: CompareToString X where
--   compareToString = show
-- ```
class CompareToString a where
  compareToString :: a -> String

instance compareToStringString :: CompareToString String where
  compareToString = id


----------
-- Component

-- NOTE: Effects must be unified among input, component, HTML, and DSL types. Not all of
-- these will be defined in the same file. As an architectural decision, I have included
-- the effects type with the component definition. You are expected, when defining anything
-- else using the component types, to import and use this effects row. For example, if you
-- define default inputs or render functions elsewhere, make sure to import these effects.
--
-- The best practice is to avoid composition of synonyms at all costs because they make
-- it extremely difficult to tell when problems and duplication arise. Also defer applying
-- effects until the last possible moment: usually, this is in an actual function definition.
-- You will see that synonyms like `ChildQuery` do not explicitly list row effects.
--
-- So while you should import and use this effects type anywhere you write functions for
-- this component specifically, you should NOT import or use this effects type for any other
-- components or components higher up the chain. This effects type should be as minimal as
-- possible.
--
-- For example, this effects row must use `DOM` and `AVAR` because it mounts components in
-- slots that themselves use those effects. But rather than compose with their effects types,
-- they are written out and applied to this component for the first time here. The same is
-- done when _this_ component is used.
--
-- This may be overly-cautious, but given how difficult it is to track down the real source
-- of effect row unification errors, I advise following this practice for our components. This
-- is especially true because effects may compile in one file, but fail to compile once actually
-- used in context.
type Effects eff = ( dom :: DOM, avar :: AVAR, console :: CONSOLE | eff )

-- The TypeaheadQuery, TypeaheadInput, ChildQuery, and component types use the same effects,
-- so make sure to apply the Effects type to each. NOTE: Avoid prematurely applying effects
-- by applying them in synonyms. Only use them in function signatures where it is necessary.
component :: ∀ o item source err eff m
  . MonadAff (Effects eff) m
 => CompareToString item
 => Eq item
 => Show err
 => H.Component
      HH.HTML
      (TypeaheadQuery o item source err (Effects eff) m)
      (TypeaheadInput o item source err (Effects eff) m)
      (TypeaheadMessage o item source err)
      m
component =
  H.lifecycleParentComponent
    { initialState
    , render: extract
    , eval
    , receiver: const Nothing
    , initializer: Just (H.action Initialize)
    , finalizer: Nothing
    }
  where
    initialState
      :: (TypeaheadInput o item source err (Effects eff) m)
      -> State o item source err (Effects eff) m
    initialState i = store i.render
      { items: i.items
      , selections: i.initialSelection
      , search: fromMaybe "" i.search
      , config: i.config
      }

    eval
      :: (TypeaheadQuery o item source err (Effects eff) m)
      ~> H.ParentDSL
          (State o item source err (Effects eff) m)
          (TypeaheadQuery o item source err (Effects eff) m)
          (ChildQuery o item (Effects eff))
          (ChildSlot)
          (TypeaheadMessage o item source err)
          m
    eval = case _ of
      HandleContainer message a -> case message of
        C.Emit query -> do
           H.raise (Emit query)
           pure a

        -- Select an item, removing it from the list of available items.
        C.ItemSelected item -> do
          (Tuple _ st) <- getState
          let (Tuple items' selections') = selectItem item st.items st.selections
          H.modify $ seeks _ { items = items', selections = selections' }
          _ <- updateContainer items'
          _ <- if st.config.keepOpen
               then pure Nothing
               else H.query' CP.cp1 ContainerSlot $ H.action $ C.Visibility C.Off
          H.raise $ ItemSelected item
          pure a

      HandleSearch message a -> case message of
        S.Emit query -> H.raise (Emit query) *> pure a
        S.ContainerQuery query -> H.query' CP.cp1 ContainerSlot query *> pure a

        -- Perform a new search, fetching data if ContinuousAsync.
        S.NewSearch text -> do
          H.modify $ seeks _ { search = text }

          (Tuple _ st) <- getState

          let applyI = applyInsertable st.config.insertable text
              applyF = applyFilter st.config.filterType text

          items' <- case st.items of
            Sync i -> pure $ Sync $ (applyI <<< applyF) i
            Async src i -> pure $ Async src $ (applyI <<< applyF) <$> i
            ContinuousAsync db _ src _ -> do
              -- ContinuousAsync may take some time to complete. Set status
              -- to Loading and request the data. When the data is fulfilled,
              -- the status will change again.
              let cont = ContinuousAsync db text src Loading
              H.modify $ seeks $ _ { items = cont }
              H.raise $ RequestData cont
              pure cont

          _ <- updateContainer items'

          H.raise $ NewSearch text
          pure a

      -- Remove a currently-selected item.
      Remove item a -> do
        (Tuple _ st) <- getState
        let (Tuple items' selections') = removeItem item st.items st.selections
        H.modify $ seeks _ { selections = selections', items = items' }
        _ <- updateContainer items'
        H.raise $ ItemRemoved item
        pure a

      -- Tell the parent what the current state of the Selections list is.
      Selections reply -> do
        (Tuple _ st) <- getState
        pure $ reply st.selections

      -- The callback: when the parent has fetched data, they'll call this to update
      -- the typeahead with that data.
      FulfillRequest items a -> do
        H.modify $ seeks $ _ { items = items }
        _ <- updateContainer items
        pure a

      -- If synchronous, do nothing; if not, request the data for the component.
      Initialize a -> do
        (Tuple _ st) <- getState

        case st.items of
          Sync _ -> pure a
          Async src _ -> do
            let async = Async src Loading
            H.modify $ seeks $ _ { items = async }
            H.raise $ RequestData async
            pure a
          ContinuousAsync db text src _ -> do
            let cont = ContinuousAsync db text src Loading
            H.modify $ seeks $ _ { items = cont }
            H.raise $ RequestData cont
            pure a

      TypeaheadReceiver input a -> do
        H.put (initialState input)
        pure a


    -- Helper function to determine how to update the container based on the item
    -- type.
    updateContainer (Sync i) = updateContainerWith (Success i)
    updateContainer (Async _ i) = updateContainerWith i
    updateContainer (ContinuousAsync _ _ _ i) = updateContainerWith i

    -- On failure, clear the items in the container and toggle it closed. We can
    -- display an error or some other information, but not using the container.
    updateContainerWith (Failure e) = do
      _ <- H.query' CP.cp1 ContainerSlot
         $ H.action
         $ C.Visibility C.Off
      _ <- H.query' CP.cp1 ContainerSlot
         $ H.action
         $ C.ReplaceItems []
      H.liftAff $ logShow e
    -- On success, replace the items in the container.
    updateContainerWith (Success items') = do
      _ <- H.query' CP.cp1 ContainerSlot
         $ H.action
         $ C.ReplaceItems items'
      pure unit
    -- On NotAsked or Loading, do nothing. The container doesn't yet need to update.
    updateContainerWith _ = pure unit



----------
-- Helpers

-- Filter items dependent on the filterable configuration.
applyFilter :: ∀ item. CompareToString item => FilterType item -> String -> Array item -> Array item
applyFilter filterType text items = case filterType of
  NoFilter -> items
  Exact -> filter (\item -> contains (Pattern text) (compareToString item)) items
  CaseInsensitive ->
    filter (\item -> contains (Pattern $ toLower text) (toLower $ compareToString item)) items
  CustomMatch match -> filter (\item -> match text item) items

-- Update items dependent on the insertable configuration.
applyInsertable :: ∀ item. CompareToString item => Insertable item -> String -> Array item -> Array item
applyInsertable insertable text items = case insertable of
  NotInsertable -> items
  Insertable mkItem | length items > 0 -> items
                    | otherwise -> (mkItem text) : items

-- Remove an item from the selections and place it in the items list.
removeItem :: ∀ item source err
  . CompareToString item
 => Eq item
 => item
 -> SyncMethod source err (Array item)
 -> SelectionType item
 -> Tuple (SyncMethod source err (Array item)) (SelectionType item)
removeItem item items selections = case selections of
  One  _  -> Tuple (insert item items) (One Nothing)
  Limit n xs  -> Tuple (insert item items) (Limit n $ filter ((/=) item) xs)
  Many xs -> Tuple (insert item items) (Many $ filter ((/=) item) xs)
    where
      insert i is = (\i' -> i : i') <$> is

-- Remove an item from the items and place it in the selections list.
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
  Limit n xs   ->
    if length xs >= n
      then (Tuple items (Limit n xs))
      else Tuple (remove item items) (Limit n $ item : xs)
    where
      insert i is = (\i' -> i : i') <$> is
      remove i is = (filter ((/=) i)) <$> is

