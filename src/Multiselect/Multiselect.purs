module CN.UI.Multiselect where

import Prelude

import Data.Array (delete, difference, mapWithIndex, (:))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Select.Dispatch (ContainerQuery(..), ContainerState, Dispatch(..), emit, getContainerProps, getItemProps, getToggleProps)
import Select.Effects (FX)
import Select.Primitive.Container as C

type State item =
  { items :: Array item
  , selection :: Array item
  , itemHTML :: item -> Array HH.PlainHTML
  }

data Query item e a
  = HandleContainer (C.Message item (Query item e) e) a
  | ItemRemoved item a
  | Receiver (MultiselectInput item) a

type MultiselectComponent item e
  = H.Component HH.HTML (Query item e) (MultiselectInput item) (MultiselectMessage item) (FX e)

type MultiselectInput item =
  { items :: Array item
  , selection :: Array item
  , itemHTML :: item -> Array HH.PlainHTML
  }

data MultiselectMessage item
  = SelectionChanged (Array item)

type ChildQuery item e = Dispatch item (Query item e) e
type ChildSlot = Unit

type MultiselectHTML item e =
  H.ParentHTML (Query item e) (ChildQuery item e) ChildSlot (FX e)

type MultiselectDSL item e =
  H.ParentDSL (State item) (Query item e) (ChildQuery item e) ChildSlot (MultiselectMessage item) (FX e)

component :: ∀ item e. Eq item => MultiselectComponent item e
component =
  H.parentComponent
    { initialState
    , render
    , eval
    , receiver: HE.input Receiver
    }
  where
    initialState :: MultiselectInput item -> State item
    initialState input = { items: input.items, itemHTML: input.itemHTML, selection: input.selection }

    render :: State item -> MultiselectHTML item e
    render st =
      HH.div_
        [ HH.ul_ (renderSelectedItem <$> st.selection)
        , HH.slot
          unit
          C.component
          { items: difference st.items st.selection, render: renderContainer st.itemHTML }
          ( HE.input HandleContainer )
        ]
      where
        renderSelectedItem :: item -> MultiselectHTML item e
        renderSelectedItem item =
          HH.div_
             $ (HH.fromPlainHTML <$> (st.itemHTML item))
            <> [HH.button [ HE.onClick $ HE.input_ (ItemRemoved item) ] [ HH.text "X" ]]

    eval :: (Query item e) ~> (MultiselectDSL item e)
    eval = case _ of

      Receiver input a -> a <$ do
        H.put $ initialState input

      ItemRemoved item a -> a <$ do
        st <- H.get
        let selection = delete item st.selection
        H.modify (_ { selection = selection })
        _ <- H.query unit
          $ H.action
          $ Container
          $ ContainerReceiver
          $ { render: renderContainer st.itemHTML
            , items: difference st.items selection }
        H.raise $ SelectionChanged selection

      HandleContainer m a -> case m of
        C.Emit q -> emit eval q a

        C.ItemSelected item -> a <$ do
          st <- H.get
          let selection = item : st.selection
          H.modify (_ { selection = selection })
          _ <- H.query unit
            $ H.action
            $ Container
            $ ContainerReceiver
            $ { render: renderContainer st.itemHTML
              , items: difference st.items selection }
          H.raise $ SelectionChanged selection

renderContainer ::
   ∀ item e
   . (item -> Array HH.PlainHTML)
  -> ContainerState item
  -> H.HTML Void (ChildQuery item e)
renderContainer itemHTML st =
  HH.div_
  $ if not st.open
    then [ renderToggle ]
    else [ renderToggle, renderItems $ renderItem `mapWithIndex` st.items ]

  where
    renderToggle :: H.HTML Void (ChildQuery item e)
    renderToggle =
      HH.div
        ( getToggleProps [] )
        [ HH.text "Placeholder"
        , HH.span_ [ HH.text "Select" ]
        ]

    renderItems :: Array (H.HTML Void (ChildQuery item e)) -> H.HTML Void (ChildQuery item e)
    renderItems html =
      HH.div
        ( getContainerProps [])
        [ HH.ul_ html ]

    renderItem :: Int -> item -> H.HTML Void (ChildQuery item e)
    renderItem index item =
      HH.li ( getItemProps index [] ) $ HH.fromPlainHTML <$> (itemHTML item)
