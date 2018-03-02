module Ocelot.Components.Dropdown where

import Prelude

import CN.UI.Core.Typeahead (SelectionChange(..))
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Aff.Console (CONSOLE)
import DOM (DOM)
import Data.Array (delete, difference, length, mapWithIndex, snoc)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(Tuple))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Select.Primitives.Container as C

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
  , title :: String
  , placeholder :: String
  , helpText :: String
  }

-- Component query definition
data Query item a
  = HandleContainer (C.Message (Query item) item) a
  | ToContainer (C.ContainerQuery (Query item) item Unit) a
  | Removed item a
  | Receiver (DropdownInput item) a

-- Component input and message types
type DropdownInput item = State item

data DropdownMessage item
  = SelectionsChanged SelectionChange item (SelectionType item)

-- Component child types
type ChildQuery item = C.ContainerQuery (Query item) item
type ChildSlot = Unit

-- Return type of render function; must use Dispatch as child type (or coproduct)
type DropdownHTML item m =
  H.ParentHTML (Query item) (ChildQuery item) ChildSlot m

-- Return type of eval function
type DropdownDSL item m =
  H.ParentDSL
    (State item)
    (Query item)
    (ChildQuery item)
    ChildSlot
    (DropdownMessage item)
    m


----------
-- Component definition
component :: ∀ item eff m
  . MonadAff ( dom :: DOM, console :: CONSOLE | eff ) m
 => Eq item
 => H.Component HH.HTML (Query item) (DropdownInput item) (DropdownMessage item) m
component =
  H.parentComponent
    { initialState
    , render
    , eval
    , receiver: HE.input Receiver
    }
  where
    initialState :: DropdownInput item -> State item
    initialState i =
      { items: i.items
      , itemHTML: i.itemHTML
      , selection: i.selection
      , placeholder: i.placeholder
      , title: i.title
      , helpText: i.helpText
      }

    render :: State item -> DropdownHTML item m
    render st =
      HH.div
      [ HP.class_ $ HH.ClassName "w-full px-3" ]
      [ HH.label
        [ HP.class_ $ HH.ClassName "block uppercase tracking-wide text-grey-darker text-xs font-bold mb-2" ]
        [ HH.text st.title ]
      , case st.selection of
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
              [ renderSelection items
              , renderMultiToggle
              , HH.slot
                  unit
                  C.component
                  { items: difference st.items items, render: renderContainer st.itemHTML }
                  ( HE.input HandleContainer )
              ]
      , HH.p
        [ HP.class_ $ HH.ClassName "mt-1 text-grey-dark text-xs" ]
        [ HH.text st.helpText ]
      ]
      where
        inputStyle :: String
        inputStyle = "rounded-sm bg-white w-full flex py-1 px-1"

        browseButtonStyle :: String
        browseButtonStyle =
          "font-medium text-blue-light border-l ml-1 px-3 text-sm cursor-pointer self-center"

        browseButton :: DropdownHTML item m
        browseButton =
          HH.p
            [ HP.class_ $ HH.ClassName browseButtonStyle ]
            [ HH.text "Browse" ]

        closeButton :: item -> DropdownHTML item m
        closeButton item =
          HH.button
            [ HE.onClick $ HE.input_ (Removed item)
            , HP.class_ $ HH.ClassName "text-grey-light float-right leading-normal invisible group-hover:visible"
            ]
            [ HH.text "✕" ]

        renderSingleToggle :: Maybe item -> DropdownHTML item m
        renderSingleToggle selected =
          HH.div
            ( C.getToggleProps
              ToContainer
              [ HP.class_ $ HH.ClassName inputStyle ]
            )
            [ HH.div
              [ HP.class_ $ HH.ClassName toggleStyle ]
              toggleHTML
            , browseButton
            ]
          where
            toggleStyle :: String
            toggleStyle = case selected of
              Nothing -> "leading-normal text-grey-dark py-1 px-3 flex-auto"
              Just _ -> "leading-normal group text-grey-darkest py-1 px-3 flex-auto hover:bg-grey-lightest"

            toggleHTML :: Array (DropdownHTML item m)
            toggleHTML = case selected of
              Nothing -> [ HH.text st.placeholder ]
              Just item -> snoc (HH.fromPlainHTML <$> (st.itemHTML item)) (closeButton item)

        renderMultiToggle :: DropdownHTML item m
        renderMultiToggle =
          HH.div
            ( C.getToggleProps
                ToContainer
                [ HP.class_ $ HH.ClassName inputStyle ]
            )
            [ HH.div
              [ HP.class_ $ HH.ClassName "leading-normal text-grey-dark py-1 px-3 flex-auto" ]
              [ HH.text st.placeholder ]
            , browseButton
            ]

        renderSelection :: Array item -> DropdownHTML item m
        renderSelection items =
          if length items == 0
          then HH.div_ []
          else
            HH.div
              [ HP.class_ $ HH.ClassName "bg-white rounded-sm w-full border-b border-grey-lighter" ]
              [ HH.ul
                [ HP.class_ $ HH.ClassName "list-reset" ]
                ( renderSelectedItem <$> items )
              ]

        renderSelectedItem :: item -> DropdownHTML item m
        renderSelectedItem item =
          HH.li
            [ HP.class_ $ HH.ClassName "px-1 py-1 text-grey-darkest" ]
            [ HH.div
              [ HP.class_ $ HH.ClassName "leading-normal group hover:bg-grey-lightest px-3 py-1" ]
              ( snoc (HH.fromPlainHTML <$> (st.itemHTML item)) (closeButton item) )
            ]


    eval :: (Query item) ~> (DropdownDSL item m)
    eval = case _ of

      Receiver input a -> a <$ do
        H.put $ initialState input

      ToContainer q a -> H.query unit q *> pure a

      Removed item a -> a <$ do
        st <- H.get

        Tuple selection items <- pure $ case st.selection of
          Single _ -> Tuple (Single Nothing) st.items
          Multi items -> do
            let containerItems = delete item items
            Tuple (Multi containerItems) (difference st.items containerItems)

        H.modify (_ { selection = selection })
        _ <- H.query unit $ H.action $ C.ReplaceItems items

        H.raise $ SelectionsChanged ItemRemoved item selection

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

          _ <- H.query unit $ H.action $ C.ReplaceItems items
          _ <- case st.selection of
            Single _ -> H.query unit $ H.action $ C.SetVisibility C.Off
            Multi _ -> (pure <<< pure) unit
          H.raise $ SelectionsChanged ItemSelected item selection

        otherwise -> pure a


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
  HH.div
    [ HP.class_ $ HH.ClassName "relative" ]
    if not st.open
      then []
      else [ renderItems $ renderItem `mapWithIndex` st.items ]
  where
    -- The individual items to render
    renderItems :: Array (H.HTML Void (ChildQuery item)) -> H.HTML Void (ChildQuery item)
    renderItems html =
      HH.div
      ( C.getContainerProps
        [ HP.class_ $ HH.ClassName "absolute bg-white shadow rounded-sm pin-t pin-l w-full" ]
      )
      [ HH.ul [ HP.class_ $ HH.ClassName "list-reset" ] html ]

    -- One particular item to render
    renderItem :: Int -> item -> H.HTML Void (ChildQuery item)
    renderItem index item =
      HH.li ( C.getItemProps index props ) $ HH.fromPlainHTML <$> (itemHTML item)
      where
        props =
          [ HP.class_
            $ HH.ClassName
            $ "leading-normal px-4 py-1 text-grey-darkest"
            <> if st.highlightedIndex == Just index then " bg-grey-lighter" else ""
          ]
