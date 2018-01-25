module CN.UI.Dropdown where

import Prelude

import Data.Array (delete, difference, mapWithIndex, snoc)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(Tuple))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Select.Effects (FX)
import Select.Primitive.Container as C

----------
-- Item types

data SelectableStatus
  = Selectable
  | NotSelectable


----------
-- Component types

-- Type of Selection
data SelectionType item
  = Single (Maybe item)
  | Multi (Array item)

-- Component state definition
type State item =
  { items :: Array item
  , selection :: SelectionType item
  , itemHTML :: item -> Array HH.PlainHTML
  }

-- Component query definition
data Query item a
  = HandleContainer (C.Message (Query item) item) a
  | ToContainer (C.ContainerQuery (Query item) item Unit) a
  | ItemRemoved item a
  | Receiver (DropdownInput item) a

-- Component top-level definition
type DropdownComponent item e
  = H.Component HH.HTML (Query item) (DropdownInput item) (DropdownMessage item) (FX e)

-- Component input and message types
type DropdownInput item = State item

data DropdownMessage item
  = SelectionChanged (SelectionType item)


-- Component child types
type ChildQuery item = C.ContainerQuery (Query item) item
type ChildSlot = Unit

-- Return type of render function; must use Dispatch as child type (or coproduct)
type DropdownHTML item e =
  H.ParentHTML (Query item) (ChildQuery item) ChildSlot (FX e)

-- Return type of eval function
type DropdownDSL item e =
  H.ParentDSL
    (State item)
    (Query item)
    (ChildQuery item)
    ChildSlot
    (DropdownMessage item)
    (FX e)


----------
-- Component definition
component :: ∀ item e. Eq item => DropdownComponent item e
component =
  H.parentComponent
    { initialState
    , render
    , eval
    , receiver: HE.input Receiver
    }
  where
    initialState :: DropdownInput item -> State item
    initialState i = { items: i.items, itemHTML: i.itemHTML, selection: i.selection }


    render :: State item -> DropdownHTML item e
    render st = case st.selection of
      Single item ->
        HH.div_
          [ renderSingleToggle item
          , HH.slot
              unit
              C.component
              { items: maybe st.items (flip delete st.items) item
              , render: renderContainer st.itemHTML
              }
              ( HE.input HandleContainer )
          ]

      Multi items ->
        HH.div_
          [ HH.ul_ (renderSelectedItem <$> items)
          , renderMultiToggle
          , HH.slot
              unit
              C.component
              { items: difference st.items items, render: renderContainer st.itemHTML }
              ( HE.input HandleContainer )
          ]
      where
        renderSingleToggle :: Maybe item -> DropdownHTML item e
        renderSingleToggle selected =
          HH.div
            ( C.getToggleProps ToContainer [] )
            ( HH.fromPlainHTML <$> ( maybe [ HH.text "Select" ] st.itemHTML selected )
              <> [ HH.button_
                   [ HH.i
                     [ HP.class_ (HH.ClassName "caret") ]
                     []
                   ]
                 ]
            )

        renderMultiToggle :: DropdownHTML item e
        renderMultiToggle =
          HH.div
            ( C.getToggleProps ToContainer [] )
            [ HH.text "Select" ]

        renderSelectedItem :: item -> DropdownHTML item e
        renderSelectedItem item =
          HH.li_
             $ ( HH.fromPlainHTML <$> (st.itemHTML item) )
            <> [ HH.button [ HE.onClick $ HE.input_ (ItemRemoved item) ] [ HH.text "X" ] ]


    eval :: (Query item) ~> (DropdownDSL item e)
    eval = case _ of

      Receiver input a -> a <$ do
        H.put $ initialState input

      ToContainer q a -> H.query unit q *> pure a

      ItemRemoved item a -> a <$ do
        st <- H.get
        Tuple selection items <- pure $ case st.selection of
          Single _ -> Tuple (Single Nothing) st.items
          Multi items -> do
            let containerItems = delete item items
            Tuple (Multi containerItems) (difference st.items containerItems)
        H.modify (_ { selection = selection })
        _ <- H.query unit
          $ H.action
          $ C.ContainerReceiver
          $ { render: renderContainer st.itemHTML
            , items
            }
        H.raise $ SelectionChanged selection

      HandleContainer m a -> case m of
        C.Emit q -> eval q *> pure a

        C.ItemSelected item -> a <$ do
          st <- H.get
          Tuple selection items <- pure $ case st.selection of
            Single _ -> Tuple (Single $ Just item) (delete item st.items)
            Multi items -> do
              let containerItems = snoc items item
              Tuple (Multi containerItems) (difference st.items containerItems)
          H.modify (_ { selection = selection })
          _ <- H.query unit
            $ H.action
            $ C.ContainerReceiver
            $ { render: renderContainer st.itemHTML
              , items
              }
          _ <- case st.selection of
            Single _ -> H.query unit
              $ H.action
              $ C.Visibility C.Off
            Multi _ -> (pure <<< pure) unit
          H.raise $ SelectionChanged selection


----------
-- Render helpers

-- The clickable region that opens the dropdown
-- Render the dropdown
renderContainer ::
   ∀ item
   . (item -> Array HH.PlainHTML)
  -> (C.ContainerState item)
  -> H.HTML Void (ChildQuery item)
renderContainer itemHTML st =
  HH.div_
  $ if not st.open
    then [ ]
    else [ renderItems $ renderItem `mapWithIndex` st.items ]
  where
    -- The individual items to render
    renderItems :: Array (H.HTML Void (ChildQuery item)) -> H.HTML Void (ChildQuery item)
    renderItems html =
      HH.div
      ( C.getContainerProps [] )
      [ HH.ul_ html ]

    -- One particular item to render
    renderItem :: Int -> item -> H.HTML Void (ChildQuery item)
    renderItem index item =
      HH.li ( C.getItemProps index [] ) $ HH.fromPlainHTML <$> (itemHTML item)
