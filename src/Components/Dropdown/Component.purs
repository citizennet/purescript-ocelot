module Ocelot.Component.Dropdown where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Store (Store, seeks, store)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Renderless.State (updateStore)
import Select as Select

data Query o item m a
  = HandleSelect (Select.Message o item) a
  | SetItems (Array item) a
  | SetSelection (Maybe item) a
  | Receive (Input o item m) a

type StateStore o item m =
  Store (State item) (H.ParentHTML (Query o item m) (ChildQuery o item) ChildSlot m)

type State item =
  { selectedItem :: Maybe item
  , items :: Array item
  }

type Input o item m =
  { selectedItem :: Maybe item
  , items :: Array item
  , render :: State item -> H.ParentHTML (Query o item m) (ChildQuery o item) ChildSlot m
  }

data Message o item
  = Selected item
  | VisibilityChanged Select.Visibility
  | Emit (o Unit)

type ChildSlot = Unit

type ChildQuery o item = Select.Query o item

component
  :: ∀ o item m
   . MonadAff m
  => H.Component HH.HTML (Query o item m) (Input o item m) (Message o item) m
component =
  H.parentComponent
    { initialState
    , render: extract
    , eval
    , receiver: const Nothing
    }

  where
    initialState :: Input o item m -> StateStore o item m
    initialState i = store i.render
      { selectedItem: i.selectedItem
      , items: i.items
      }

    eval
      :: Query o item m
      ~> H.ParentDSL
          (StateStore o item m)
          (Query o item m)
          (ChildQuery o item)
          (ChildSlot)
          (Message o item)
          m
    eval = case _ of
      HandleSelect message a -> case message of
        Select.Selected item -> do
          _ <- H.query unit $ Select.setVisibility Select.Off
          H.modify_ $ seeks _ { selectedItem = Just item }
          H.raise $ Selected item
          pure a
        Select.Emit query -> H.raise (Emit query) $> a
        Select.VisibilityChanged vis -> H.raise (VisibilityChanged vis) $> a
        _ -> pure a
      SetItems items a -> do
        H.modify_ $ seeks _ { items = items }
        void $ H.query unit $ Select.replaceItems items
        pure a
      SetSelection item a -> do
        H.modify_ $ seeks _ { selectedItem = item }
        pure a
      Receive input a -> do
        H.modify_ $ updateStore input.render identity
        pure a

