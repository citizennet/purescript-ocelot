-- | The typeahead module intended for imports
module Ocelot.Typeahead
  ( Action
  , ChildSlots
  , Component
  , ComponentHTML
  , ComponentRender
  , ComponentM
  , CompositeAction
  , CompositeComponent
  , CompositeComponentHTML
  , CompositeComponentM
  , CompositeComponentRender
  , CompositeInput
  , CompositeQuery
  , CompositeState
  , DefaultAsyncTypeaheadInput
  , DefaultSyncTypeaheadInput
  , EmbeddedAction(..)
  , EmbeddedChildSlots
  , Input 
  , Insertable(..)
  , Operations
  , Output(..)
  , Query(..)
  , SelectionCause(..)
  , Slot
  , Spec
  , State
  , StateRow
  , StateStore
  , asyncMulti
  , asyncSingle
  , component
  , defRenderContainer
  , disabledClasses
  , inputProps
  , isDisabled
  , linkClasses
  , multi
  , renderError
  , renderHeaderSearchDropdown
  , renderMulti
  , renderSearchDropdown
  , renderSingle
  , renderToolbarSearchDropdown
  , single
  , spinner
  , syncMulti
  , syncSingle
  ) where

import Prelude
import Control.Alternative (class Plus, empty)
import Control.Comonad (extract)
import Control.Comonad.Store (Store, store)
import DOM.HTML.Indexed (HTMLinput)
import Data.Array ((!!), (:), foldr)
import Data.Array as Array
import Data.Fuzzy (Fuzzy(..), match)
import Data.Fuzzy as Fuzzy
import Data.Maybe (Maybe(..), isJust, isNothing, maybe)
import Data.Newtype (unwrap)
import Data.Rational ((%))
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff.Class (class MonadAff)
import Foreign.Object (Object)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (Prop(..), PropValue)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..), isFailure, isLoading)
import Ocelot.Block.Button as Button
import Ocelot.Block.Conditional (conditional)
import Ocelot.Block.Format as Format
import Ocelot.Block.Icon as Icon
import Ocelot.Block.Input as Input
import Ocelot.Block.ItemContainer as IC
import Ocelot.Block.Loading as Loading
import Ocelot.HTML.Properties (css, (<&>))
import Renderless.State(modifyStore_)
import Select as S
import Select.Setters as Setters
import Type.Data.Symbol (SProxy(..))
import Unsafe.Coerce (unsafeCoerce)

--------
-- Types

data Action action (f :: Type -> Type) item (m :: Type -> Type)
  = PassingOutput (Output action f item)
  | ReceiveRender (Input action f item m)

type ChildSlots action f item =
  ( select :: S.Slot (Query f item) EmbeddedChildSlots (Output action f item) Unit
  )

type Component action f item m 
  = H.Component HH.HTML (Query f item) (Input action f item m) (Output action f item) m

type ComponentHTML action f item m 
  = H.ComponentHTML (Action action f item m) (ChildSlots action f item) m

type ComponentRender action f item m 
  = State f item m -> ComponentHTML action f item m

type ComponentM action f item m a 
  = H.HalogenM (StateStore action f item m) (Action action f item m) (ChildSlots action f item) (Output action f item) m a

type CompositeAction action f item m = S.Action (EmbeddedAction action f item m)

type CompositeComponent action f item m = H.Component HH.HTML (CompositeQuery f item) (CompositeInput f item m) (Output action f item) m

type CompositeComponentHTML action f item m = H.ComponentHTML (CompositeAction action f item m) EmbeddedChildSlots m

type CompositeComponentM action f item m a = H.HalogenM (CompositeState f item m) (CompositeAction action f item m) EmbeddedChildSlots (Output action f item) m a

type CompositeComponentRender action f item m = (CompositeState f item m) -> CompositeComponentHTML action f item m

type CompositeInput f item m = S.Input (StateRow f item m)

type CompositeQuery f item = S.Query (Query f item) EmbeddedChildSlots

type CompositeState f item m = S.State (StateRow f item m)

type DefaultAsyncTypeaheadInput item m =
  { itemToObject :: item -> Object String
  , renderFuzzy :: Fuzzy item -> HH.PlainHTML
  , async :: String -> m (RemoteData String (Array item))
  }

type DefaultSyncTypeaheadInput item =
  { itemToObject :: item -> Object String
  , renderFuzzy :: Fuzzy item -> HH.PlainHTML
  }

data EmbeddedAction action (f :: Type -> Type) item (m :: Type -> Type)
  = Initialize
  | Remove item
  | RemoveAll
  | Raise action

type EmbeddedChildSlots = () -- NOTE no extension

type Input action f item m =
  { items :: RemoteData String (Array item)
  , insertable :: Insertable item
  , keepOpen :: Boolean
  , itemToObject :: item -> Object String
  , async :: Maybe (String -> m (RemoteData String (Array item)))
  , disabled :: Boolean
  , debounceTime :: Maybe Milliseconds
  , render :: CompositeComponentRender action f item m
  }

data Insertable item
  = NotInsertable
  | Insertable (String -> item)

type Operations f item =
  { runSelect  :: item -> f item -> f item
  , runRemove  :: item -> f item -> f item
  , runFilter  :: Array item -> f item -> Array item
  }

data Output action (f :: Type -> Type) item
  = Searched String
  | Selected item
  | SelectionChanged SelectionCause (f item)
  | Emit action

data Query f item a
  = GetSelected (f item -> a)
  | ReplaceSelected (f item) a
  | ReplaceSelectedBy (Array item -> f item) a
  | ReplaceItems (RemoteData String (Array item)) a
  | Reset a
  | SetDisabled Boolean a

data SelectionCause
  = RemovalQuery
  | ReplacementQuery
  | ResetQuery
  | SelectionMessage

derive instance eqSelectionCause :: Eq SelectionCause

type Slot action f item id = H.Slot (Query f item) (Output action f item) id

type Spec action f item m = S.Spec (StateRow f item m) (Query f item) (EmbeddedAction action f item m) EmbeddedChildSlots (CompositeInput f item m) (Output action f item) m

type State f item m = Record (StateRow f item m)

type StateRow f item m =
  ( items :: RemoteData String (Array item) -- NOTE pst.items, Parent(Typeahead)
  , insertable :: Insertable item
  , keepOpen :: Boolean
  , itemToObject :: item -> Object String
  , async :: Maybe (String -> m (RemoteData String (Array item)))
  , disabled :: Boolean
  , ops :: Operations f item
  , config :: { debounceTime :: Maybe Milliseconds }
  , selected :: f item
  , fuzzyItems :: Array (Fuzzy item) -- NOTE cst.items, Child(Select)
  )

type StateStore action f item m = Store (State f item m) (ComponentHTML action f item m)

-------------
-- Components

component
  :: forall action f item m
  . Plus f
  => Eq item
  => MonadAff m
  => Operations f item
  -> Component action f item m
component ops = H.mkComponent
  { initialState: initialState ops
  , render: extract
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , handleQuery = handleQuery
      }
  }

single
  :: forall action item m
  . Eq item
  => MonadAff m
  => Component action Maybe item m
single = component
  { runSelect: const <<< Just
  , runRemove: const (const Nothing)
  , runFilter: \items -> maybe items (\i -> Array.filter (_ /= i) items)
  }

multi
  :: forall action item m
  . Eq item
  => MonadAff m
  => Component action Array item m
multi = component
  { runSelect: (:)
  , runRemove: Array.filter <<< (/=)
  , runFilter: Array.difference
  }

asyncSingle
  :: ∀ action item m
   . Eq item
  => MonadAff m
  => DefaultAsyncTypeaheadInput item m
  -> Array (HH.IProp HTMLinput (CompositeAction action Maybe item m))
  -> Input action Maybe item m
asyncSingle { async, itemToObject, renderFuzzy } props =
  { items: NotAsked
  , insertable: NotInsertable
  , keepOpen: false
  , itemToObject
  , debounceTime: Just $ Milliseconds 300.0
  , async: Just async
  , disabled: isDisabled props
  , render: renderSingle
      props
      (renderFuzzy <<< match false itemToObject "")
      (defRenderContainer renderFuzzy)
  }

asyncMulti
  :: ∀ action item m
   . Eq item
  => MonadAff m
  => DefaultAsyncTypeaheadInput item m
  -> Array (HH.IProp HTMLinput (CompositeAction action Array item m))
  -> Input action Array item m
asyncMulti { async, itemToObject, renderFuzzy } props =
  { items: NotAsked
  , insertable: NotInsertable
  , keepOpen: true
  , itemToObject
  , debounceTime: Just $ Milliseconds 300.0
  , async: Just async
  , disabled: isDisabled props
  , render: renderMulti
      props
      (renderFuzzy <<< match false itemToObject "")
      (defRenderContainer renderFuzzy)
  }

syncSingle
  :: ∀ action item m
   . Eq item
  => MonadAff m
  => DefaultSyncTypeaheadInput item
  -> Array (HH.IProp HTMLinput (CompositeAction action Maybe item m))
  -> Input action Maybe item m
syncSingle { itemToObject, renderFuzzy } props =
  { items: NotAsked
  , insertable: NotInsertable
  , keepOpen: false
  , itemToObject
  , debounceTime: Nothing
  , async: Nothing
  , disabled: isDisabled props
  , render: renderSingle
      props
      (renderFuzzy <<< match false itemToObject "")
      (defRenderContainer renderFuzzy)
  }

syncMulti
  :: ∀ action item m
   . Eq item
  => MonadAff m
  => DefaultSyncTypeaheadInput item
  -> Array (HH.IProp HTMLinput (CompositeAction action Array item m))
  -> Input action Array item m
syncMulti { itemToObject, renderFuzzy } props =
  { items: NotAsked
  , insertable: NotInsertable
  , keepOpen: true
  , itemToObject
  , debounceTime: Nothing
  , async: Nothing
  , disabled: isDisabled props
  , render: renderMulti
      props
      (renderFuzzy <<< match false itemToObject "")
      (defRenderContainer renderFuzzy)
  }

---------
-- Values

_select = SProxy :: SProxy "select"

applyInsertable
  :: forall item
  . (item -> Fuzzy item)
  -> Insertable item
  -> String
  -> Array (Fuzzy item)
  -> Array (Fuzzy item)
applyInsertable _ _ "" items = items
applyInsertable match insertable text items = case insertable of
  NotInsertable -> items
  Insertable mkItem | Array.length (Array.filter isExactMatch items) > 0 -> items
                    | otherwise -> (match $ mkItem text) : items
  where
    isExactMatch (Fuzzy { distance }) = distance == Fuzzy.Distance 0 0 0 0 0 0

defRenderContainer
  :: ∀ action f item m
   . (Fuzzy item -> HH.PlainHTML)
  -> CompositeComponentRender action f item m
defRenderContainer renderFuzzy st =
  IC.itemContainer st.highlightedIndex (renderFuzzy <$> st.fuzzyItems) []

disabledClasses :: Array HH.ClassName
disabledClasses = HH.ClassName <$>
  [ "bg-grey-95"
  , "text-grey-70"
  , "sibling:bg-grey-95"
  , "sibling:text-grey-50"
  , "border-t-2"
  , "border-b-2"
  , "font-light"
  , "focus:no-outline"
  , "py-2"
  , "border-l-2"
  , "w-full"
  , "px-3"
  ]

embeddedHandleAction
  :: forall action f item m
  . Eq item
  => Plus f
  => MonadAff m
  => EmbeddedAction action f item m
  -> CompositeComponentM action f item m Unit
embeddedHandleAction = case _ of
  Initialize -> do
    synchronize
  Remove item -> do
    st <- H.modify \st -> st { selected = st.ops.runRemove item st.selected }
    H.raise $ SelectionChanged RemovalQuery st.selected
    synchronize
  RemoveAll -> do
    st <- H.modify \st ->
      st { selected = empty :: f item
         , visibility = S.Off
         }
    H.raise $ SelectionChanged RemovalQuery st.selected
    synchronize
  Raise action -> do
    H.raise $ Emit action

embeddedHandleMessage
  :: forall action f item m
   . Eq item
  => MonadAff m
  => S.Event
  -> CompositeComponentM action f item m Unit
embeddedHandleMessage = case _ of
  S.Selected idx -> do
    { fuzzyItems } <- H.get
    case fuzzyItems !! idx of
      Nothing -> pure unit
      Just (Fuzzy { original: item }) -> do
        st <- H.modify \st -> st { selected = st.ops.runSelect item st.selected }
        when (not st.keepOpen) do
          H.modify_ _ { visibility = S.Off }
        H.raise $ SelectionChanged SelectionMessage st.selected
        H.raise $ Selected item
        synchronize
  -- Perform a new search, fetching data if Async.
  S.Searched text -> do
    H.modify_ _ { search = text }
    async <- H.gets _.async
    case async of
      Nothing -> pure unit
      Just fetchItems -> do
        H.modify_ _ { items = Loading }
        synchronize
        newItems <- H.lift $ fetchItems text
        H.modify_ _ { items = newItems }
    H.raise $ Searched text
    synchronize
  _ -> pure unit

embeddedHandleQuery
  :: forall action f item m a
  . Plus f
  => Eq item
  => MonadAff m
  => Query f item a
  -> CompositeComponentM action f item m (Maybe a)
embeddedHandleQuery = case _ of
  GetSelected reply -> do
    { selected } <- H.get
    pure $ Just $ reply selected
  ReplaceSelected selected a -> Just a <$ do
    replaceSelected selected
  ReplaceSelectedBy f a -> Just a <$ do
    { items } <- H.get
    case items of
      Success items' -> replaceSelected (f items')
      _ -> pure unit
  ReplaceItems items a -> Just a <$ do
    H.modify_ _ { items = items }
    synchronize
  Reset a -> Just a <$ do
    st <- H.modify _ { selected = empty :: f item, items = NotAsked }
    H.raise $ SelectionChanged ResetQuery st.selected
    synchronize
  SetDisabled disabled a -> Just a <$ do
    H.modify_ _ { disabled = disabled }

embeddedInitialize :: forall action f item m. Maybe (EmbeddedAction action f item m)
embeddedInitialize = Just Initialize

-- NOTE configure Select
embeddedInput :: forall f item m. State f item m -> CompositeInput f item m
embeddedInput { items, selected, insertable, keepOpen, itemToObject, ops, async, fuzzyItems, config: { debounceTime }, disabled } =
  { inputType: S.Text
  , search: Nothing
  , debounceTime
  , getItemCount: Array.length <<< _.fuzzyItems
  , items
  , selected
  , insertable
  , keepOpen
  , itemToObject
  , ops
  , async
  , fuzzyItems
  , disabled
  , config: { debounceTime } -- NOTE overhead
  }

getNewItems
  :: forall f item m
  . MonadAff m
  => Eq item
  => CompositeState f item m
  -> RemoteData String (Array (Fuzzy item))
getNewItems st =
  Array.sort
  <<< applyF
  <<< applyI
  <<< fuzzyItems
  <$> (map (flip st.ops.runFilter st.selected) st.items)
  where
    matcher :: item -> Fuzzy item
    matcher = Fuzzy.match true st.itemToObject st.search

    fuzzyItems :: Array item -> Array (Fuzzy item)
    fuzzyItems = map matcher

    applyI :: Array (Fuzzy item) -> Array (Fuzzy item)
    applyI = applyInsertable matcher st.insertable st.search

    applyF :: Array (Fuzzy item) -> Array (Fuzzy item)
    applyF = Array.filter (\(Fuzzy { ratio }) -> ratio > (2 % 3))

-- NOTE re-raise output messages from the embedded component
-- NOTE update Dropdown render function if it relies on external state
handleAction
  :: forall action f item m
  . Plus f
  => Eq item
  => MonadAff m
  => Action action f item m
  -> ComponentM action f item m Unit
handleAction = case _ of
  PassingOutput output ->
    H.raise output
  ReceiveRender { render } -> do
    modifyStore_ (renderAdapter render) identity

-- NOTE passing query to the embedded component
handleQuery :: forall action f item m a. Query f item a -> ComponentM action f item m (Maybe a)
handleQuery = case _ of
  GetSelected reply -> do
    response <- H.query _select unit (S.Query $ H.request GetSelected)
    pure $ reply <$> response
  ReplaceSelected selected a -> Just a <$ do
    H.query _select unit (S.Query $ H.tell $ ReplaceSelected selected)
  ReplaceSelectedBy f a -> Just a <$ do
    H.query _select unit (S.Query $ H.tell $ ReplaceSelectedBy f)
  ReplaceItems items a -> Just a <$ do
    H.query _select unit (S.Query $ H.tell $ ReplaceItems items)
  Reset a -> Just a <$ do
    H.query _select unit (S.Query $ H.tell $ Reset)
  SetDisabled disabled a -> Just a <$ do
    H.query _select unit (S.Query $ H.tell $ SetDisabled disabled)

initialState
  :: forall action f item m
  . Plus f
  => Eq item
  => MonadAff m
  => Operations f item
  -> Input action f item m
  -> StateStore action f item m
initialState ops
  { items, insertable, keepOpen, itemToObject, async, debounceTime, render, disabled }
  = store (renderAdapter render)
      { items
      , insertable
      , keepOpen
      , itemToObject
      , async
      , disabled
      , ops
      , config: {debounceTime}
      , selected: empty :: f item
      , fuzzyItems: []
      }

inputProps
  :: ∀ action f item m
   . Boolean
  -> Array (HP.IProp HTMLinput (CompositeAction action f item m))
  -> Array (HP.IProp HTMLinput (CompositeAction action f item m))
inputProps disabled iprops = if disabled
  then iprops'
  else Setters.setInputProps iprops'
  where
    iprops' = [ HP.disabled disabled, HP.autocomplete false, css "focus:next:text-blue-88" ] <&> iprops

isDisabled :: ∀ i. Array (HH.IProp HTMLinput i) -> Boolean
isDisabled = foldr f false
  where
    f (HP.IProp (Property "disabled" disabled)) | coercePropValue disabled == true = (||) true
    f _ = (||) false

    coercePropValue :: PropValue -> Boolean
    coercePropValue = unsafeCoerce

linkClasses :: Boolean -> Array HH.ClassName
linkClasses = if _
  then HH.ClassName <$> [ "text-grey-70", "no-underline", "font-medium" ]
  else Format.linkClasses

renderAdapter
  :: forall action f item m
  . Plus f
  => Eq item
  => MonadAff m
  => CompositeComponentRender action f item m
  -> ComponentRender action f item m
renderAdapter render state =
  HH.slot _select unit (S.component identity $ spec render)
    (embeddedInput state)
    (Just <<< PassingOutput)

renderError :: ∀ p i. Boolean -> HH.HTML p i
renderError error =
  conditional error
    [ css "flex items-center mt-1" ]
    [ Icon.error
      [ css "text-2xl text-yellow" ]
    , HH.p
      [ css "ml-3 text-grey-50 font-light" ]
      [ HH.text "Some data could not be retrieved here." ]
    ]

renderHeaderSearchDropdown
  :: ∀ action item m
   . Eq item
  => String
  -> String
  -> (item -> HH.PlainHTML)
  -> (Fuzzy item -> HH.PlainHTML)
  -> CompositeComponentRender action Maybe item m
renderHeaderSearchDropdown defaultLabel resetLabel renderItem renderFuzzy st =
  renderSearchDropdown resetLabel label renderFuzzy st
  where
    label = HH.span
      [ css "text-white text-3xl font-thin cursor-pointer whitespace-no-wrap" ]
      [ maybe (HH.text defaultLabel) (HH.fromPlainHTML <<< renderItem) st.selected
      , Icon.collapse [ css "ml-3 text-xl text-grey-50 align-middle" ]
      ]

renderMulti
  :: ∀ action item m
  . Array (HP.IProp HTMLinput (CompositeAction action Array item m))
  -> (item -> HH.PlainHTML)
  -> CompositeComponentRender action Array item m
  -> CompositeComponentRender action Array item m
renderMulti iprops renderItem renderContainer st =
  HH.div
    [ css "relative" ]
    [ if (not disabled && not Array.null st.selected)
        then
          HH.a
            [ css "absolute -mt-7 pin-r underline text-grey-70 cursor-pointer"
            , HE.onClick $ const <<< Just <<< S.Action $ RemoveAll
            ]
            [ HH.text "Remove All" ]
        else
          HH.text ""
    , IC.selectionContainer $ st.selected <#>
        if disabled
          then
            HH.fromPlainHTML <<< renderItem
          else
            \selected ->
              IC.selectionGroup
                renderItem
                []
                [ HE.onClick $ const <<< Just <<< S.Action $ Remove selected ]
                selected
    , Input.inputGroup_
      [ Input.inputCenter $ inputProps disabled iprops
      , Input.addonLeft_
        [ Icon.search_ ]
      , Input.addonCenter
        [ css $ if isLoading st.items then "" else "offscreen" ]
        [ spinner ]
      , Input.borderRight
        [ HP.classes $ linkClasses disabled ]
        [ HH.text "Browse" ]
      ]
    , conditional (st.visibility == S.On)
        [ css "relative block" ]
        [ renderContainer st ]
    , renderError $ isFailure st.items
    ]
  where
  disabled = st.disabled

renderSearchDropdown
  :: ∀ action item m
   . Eq item
  => String
  -> HH.PlainHTML
  -> (Fuzzy item -> HH.PlainHTML)
  -> CompositeComponentRender action Maybe item m
renderSearchDropdown resetLabel label renderFuzzy st =
  HH.label
    [ css "relative" ]
    [ HH.fromPlainHTML label
    , HH.div
      [ HP.classes
        $ HH.ClassName "min-w-80" :
          if st.visibility == S.Off
            then [ HH.ClassName "offscreen" ]
            else []
      ]
      [ IC.dropdownContainer
        [ renderInput, renderReset ]
        renderFuzzy
        ((==) st.selected <<< Just <<< _.original <<< unwrap)
        st.fuzzyItems
        st.highlightedIndex
      ]
    ]
  where
  renderInput =
    HH.div
      [ css "m-4 border-b-2 border-blue-88 pb-2 flex" ]
      [ Icon.search [ css "mr-4 text-xl text-grey-70" ]
      , HH.input
        $ inputProps false [ css "no-outline w-full", HP.placeholder "Search" ]
      ]

  renderReset =
    IC.dropdownItem
      HH.div
      [ HE.onClick $ const <<< Just <<< S.Action $ RemoveAll
      ]
      [ HH.text resetLabel ]
      ( isNothing st.selected )
      false

renderSingle
  :: ∀ action item m
  . Array (HP.IProp HTMLinput (CompositeAction action Maybe item m))
  -> (item -> HH.PlainHTML)
  -> CompositeComponentRender action Maybe item m
  -> CompositeComponentRender action Maybe item m
renderSingle iprops renderItem renderContainer st =
  HH.div_
    [ Input.inputGroup' HH.div
      [ css $ if showSelected then "" else "offscreen" ]
      [ if disabled
          then
            maybe (HH.text "")
              ( \selected -> HH.div
                [ HP.classes disabledClasses ]
                [ HH.fromPlainHTML $ renderItem selected ]
              )
            st.selected
          else
            maybe (HH.text "")
            ( \selected -> HH.div
              [ HP.classes Input.mainLeftClasses ]
              [ IC.selectionGroup renderItem
                [ HE.onClick $ Just <<< S.ToggleClick ]
                [ HE.onClick $ const <<< Just <<< S.Action $ Remove selected ]
                selected
              ])
            st.selected
      , Input.borderRight
        [ HP.classes $ linkClasses disabled
        , HE.onClick $ Just <<< S.ToggleClick
        ]
        [ HH.text "Change" ]
      ]
    , Input.inputGroup
      [ css $ if showSelected then "offscreen" else "" ]
      [ Input.inputCenter $ inputProps disabled iprops
      , Input.addonLeft_
        [ Icon.search_ ]
      , Input.addonCenter
        [ css $ if isLoading st.items then "" else "offscreen" ]
        [ spinner ]
      , Input.borderRight
        [ HP.classes $ linkClasses disabled ]
        [ HH.text "Browse" ]
      ]
    , conditional (st.visibility == S.On)
        [ css "relative block" ]
        [ renderContainer st ]
    , renderError $ isFailure st.items
    ]
  where
  disabled = st.disabled
  showSelected = isJust st.selected && st.visibility == S.Off

renderToolbarSearchDropdown
  :: ∀ action item m
   . Eq item
  => String
  -> String
  -> (item -> HH.PlainHTML)
  -> (Fuzzy item -> HH.PlainHTML)
  -> CompositeComponentRender action Maybe item m
renderToolbarSearchDropdown defaultLabel resetLabel renderItem renderFuzzy st =
  renderSearchDropdown resetLabel label renderFuzzy st
  where
    label = IC.dropdownButton
      HH.span
      [ HP.classes
        $ HH.ClassName "whitespace-no-wrap"
        : Button.buttonMainClasses
        <> Button.buttonClearClasses
      ]
      [ maybe (HH.text defaultLabel) (HH.fromPlainHTML <<< renderItem) st.selected ]

replaceSelected
  :: forall action f item m
  . Eq item
  => MonadAff m
  => f item
  -> CompositeComponentM action f item m Unit
replaceSelected selected = do
  st <- H.modify _ { selected = selected }
  H.raise $ SelectionChanged ReplacementQuery st.selected
  synchronize

spec
  :: forall action f item m
  . Plus f
  => Eq item
  => MonadAff m
  => CompositeComponentRender action f item m
  -> Spec action f item m
spec embeddedRender =
  S.defaultSpec
  { render = embeddedRender
  , handleAction = embeddedHandleAction
  , handleQuery = embeddedHandleQuery
  , handleEvent = embeddedHandleMessage
  , initialize = embeddedInitialize
  }

spinner :: ∀ p i. HH.HTML p i
spinner = Loading.spinner [ css "w-6 text-blue-88" ]

synchronize
  :: forall action f item m
  . Eq item
  => MonadAff m
  => CompositeComponentM action f item m Unit
synchronize = do
  st <- H.get
  case getNewItems st of
    Success items -> do
      H.modify_ _ { fuzzyItems = items }
    Failure err -> do
      H.modify_
        _ { visibility = S.Off
          , fuzzyItems = []
          }
    NotAsked -> do
      H.modify_
        _ { visibility = S.Off
          , fuzzyItems = []
          }
    Loading -> do
      H.modify_ _ { fuzzyItems = [] }
