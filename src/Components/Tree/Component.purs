module Ocelot.Component.Tree where

import Prelude

import Data.Array as A
import Data.Lens (class Wander, Lens', Optic', over, set)
import Data.Lens.Index (ix) as Lens
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(..), fst)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Ocelot.Block.Checkbox as Checkbox
import Ocelot.Block.Conditional as Conditional
import Ocelot.Block.Icon as Icon
import Ocelot.Data.Tree (ItemPath, Node(..), IndexPath, _expanded, _selected, _children)
import Ocelot.HTML.Properties (css)

type State item =
  { items :: Array (Node item)
  , initial :: Array (Node item)
  , renderItem :: item -> HH.PlainHTML
  , checkable :: item -> Boolean
  }

data Action item
  = ToggleItem item (ItemPath item) IndexPath Boolean
  | ToggleChildren IndexPath

data Query item a
  = SetItems (Array (Node item)) a
  | SetSelections (Array (ItemPath item)) a

type Input item =
  { renderItem :: item -> HH.PlainHTML
  , checkable :: item -> Boolean
  }

data Message item
  = ItemAdded item (ItemPath item)
  | ItemRemoved item (ItemPath item)

component :: ∀ m item.
  MonadAff m =>
  Eq item =>
  H.Component (Query item) (Input item) (Message item) m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval (H.defaultEval { handleAction = handleAction, handleQuery = handleQuery })
    }
  where
    initialState { renderItem, checkable } =
      { items: []
      , initial: []
      , renderItem
      , checkable
      }

render :: ∀ m item.
  MonadAff m =>
  Eq item =>
  State item ->
  H.ComponentHTML (Action item) () m
render { items, renderItem, checkable } =
  HH.div_ $ A.concat $ A.mapWithIndex (renderRow 0 [] []) items
  where
    renderRow depth indexPath itemPath ix (Node { selected, expanded, children, value }) =
      [ HH.div
        [ css $ "flex border-b py-2 pr-2 " <> ("pl-" <> (show (depth * 10))) ]
        [ renderCarat children expanded path
        , HH.div
          [ css "inline-flex" ]
          [ Conditional.alt_ (checkable value)
            [ Checkbox.checkbox_
              [ HE.onChecked $ ToggleItem value itemPath (A.cons ix indexPath)
              , HP.checked selected
              ]
              [ HH.fromPlainHTML $ renderItem value ]
            ]
            [ HH.span
              [ css "cursor-pointer"
              , HE.onClick \_ -> ToggleChildren path
              ]
              [ HH.fromPlainHTML $ renderItem value ]
            ]
          ]
        ]
      ] <>
      ( if not expanded then [] else
        [ HH.div_
          ( A.concat
            $ A.mapWithIndex
              (renderRow (depth + 1) (A.cons ix indexPath) (A.snoc itemPath value))
              children
          )
        ]
      )
      where
        path = A.cons ix indexPath

    renderCarat children expanded path =
      carat
        [ HE.onClick \_ -> ToggleChildren path
        , css $ "mr-3 text-xl align-text-bottom cursor-pointer " <> visible
        ]
      where
        carat = if expanded then Icon.caratDown else Icon.caratRight
        visible = if A.length children > 0 then "visible" else "invisible"

handleAction :: forall item m.
  MonadAff m =>
  Action item ->
  H.HalogenM (State item) (Action item) () (Message item) m Unit
handleAction = case _ of
  ToggleItem item itemPath indexPath checked -> do
    let pathLens = pathToLens indexPath _selected
    traverse_ (\l -> H.modify $ set l checked) pathLens
    if checked
      then
      H.raise (ItemAdded item itemPath)
      else
      H.raise (ItemRemoved item itemPath)

  ToggleChildren indexPath -> do
    let pathLens = pathToLens indexPath _expanded
    traverse_ (\l -> H.modify $ over l not) pathLens


handleQuery :: forall item m a.
  Eq item =>
  Query item a ->
  H.HalogenM (State item) (Action item) () (Message item) m (Maybe a)
handleQuery = case _ of
  SetItems items a -> do
    H.modify_ _ { items = items, initial = items }
    pure $ Just a

  SetSelections itemPaths a -> do
    { items, initial } <- H.get
    let paths = flip itemPathToIndexPath items <$> itemPaths
        updates = (\p r -> r { items = expandPath p r.items }) <$> paths
        updater = A.foldl (>>>) (_ { items = initial }) updates
    H.modify_ updater
    pure $ Just a

-----
-- Helper functions for expanding paths, toggling checkboxes, etc.

_items :: ∀ item. Lens' (State item) (Array (Node item))
_items = prop (SProxy :: SProxy "items")

pathToLens
  :: ∀ p a
   . Wander p
  => IndexPath
  -> Optic' p (Node a) Boolean
  -> Maybe (Optic' p (State a) Boolean)
pathToLens path lastProp = (<<<) _items <$> pathToLens'
  where
    pathToLens' :: Maybe (Optic' p (Array (Node a)) Boolean)
    pathToLens' = A.foldl foldLens <$> (last <$> A.head path) <*> A.tail path

    foldLens :: Optic' p (Array (Node a)) Boolean -> Int -> Optic' p (Array (Node a)) Boolean
    foldLens l ix = Lens.ix ix <<< _children <<< l

    last :: Int -> Optic' p (Array (Node a)) Boolean
    last ix = Lens.ix ix <<< lastProp

-- TODO : update this to use lenses, possibly using pathToLens on increasing subsections of array
--   e.g. [pathToLens [0] _expanded, pathToLens [0, 2] _expanded, pathToLens [0, 2, 1] _checked]
expandPath :: ∀ a. IndexPath -> Array (Node a) -> Array (Node a)
expandPath path traits = do
  expandPath' (A.head path) (A.tail path) traits
  where
    expandPath' (Just ix) (Just p) ts | A.length p > 0 = fromMaybe [] $ A.modifyAt ix (expand p) ts
                                      | otherwise = fromMaybe [] $ A.modifyAt ix check ts
    expandPath' _ _ ts = ts
    expand p (Node t) = Node $ t
      { expanded = true
      , children = expandPath' (A.head p) (A.tail p) t.children
      }
    check (Node t) = Node $ t { selected = true }

itemPathToIndexPath :: ∀ a. Eq a => ItemPath a -> Array (Node a) -> IndexPath
itemPathToIndexPath path ns =
  fst $ A.foldl makePath (Tuple [] ns) path
  where
    makePath (Tuple path' ns') item =
      Tuple
        (fromMaybe [] $ A.snoc path' <$> ix)
        (fromMaybe [] $ (_.children <<< unwrap) <$> (A.index ns' =<< ix))
      where
        ix = A.findIndex (\(Node node) -> node.value == item) ns'
