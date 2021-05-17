module UIGuide.Component.Modals where

import Prelude
import Data.Foldable (traverse_)
import Data.Map as Data.Map
import Data.Maybe (Maybe(..), isJust)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Ocelot.Block.Button as Button
import Ocelot.Block.Card as Card
import Ocelot.Block.FormField as FormField
import Ocelot.Block.Format as Format
import Ocelot.Block.Icon as Icon
import Ocelot.Block.ItemContainer (boldMatches) as IC
import Ocelot.Block.Toast as Ocelot.Block.Toast
import Ocelot.HTML.Properties (css)
import Ocelot.Part.Modal as Modal
import Ocelot.Part.Panel as Panel
import Ocelot.Typeahead as TA
import Type.Proxy (Proxy(..))
import UIGuide.Block.Backdrop as Backdrop
import UIGuide.Block.Documentation as Documentation
import UIGuide.Utility.Async as Async
import Web.UIEvent.KeyboardEvent as KE

type Component m = H.Component Query Input Message m

type ComponentHTML m = H.ComponentHTML Action ChildSlot m

type ComponentM m a = H.HalogenM State Action ChildSlot Message m a

data ComponentType
  = Modal
  | PanelLeft
  | PanelRight

derive instance eqComponentType :: Eq ComponentType
derive instance ordComponentType :: Ord ComponentType

type State =
  { ids :: Data.Map.Map ComponentType H.SubscriptionId
  , toast :: Boolean
  }

data Query a

data Action
  = Close ComponentType
  | HandleKey ComponentType KE.KeyboardEvent
  | Open ComponentType
  | Toast

type Input = Unit

type Message = Void

type ChildSlot =
  ( cp1 :: TA.Slot Action Array Async.Location String
  , cp2 :: TA.Slot Action Array Async.User String
  )

_cp1 = Proxy :: Proxy "cp1"
_cp2 = Proxy :: Proxy "cp2"

component ::
  forall m.
  MonadAff m =>
  Component m
component =
  H.mkComponent
    { initialState
    , render
    , eval:
      H.mkEval
        H.defaultEval
          { handleAction = handleAction
          }
    }

initialState :: Input -> State
initialState _ =
  { ids: Data.Map.empty
  , toast: false
  }

handleAction ::
  forall m.
  MonadAff m =>
  Action ->
  ComponentM m Unit
handleAction = case _ of
  Close ct -> close ct
  HandleKey ct ev -> handleKey ct ev
  Open ct -> open ct
  Toast -> toast

handleKey ::
  forall m.
  MonadAff m =>
  ComponentType ->
  KE.KeyboardEvent ->
  ComponentM m Unit
handleKey ct ev = do
  id <- H.gets (Data.Map.lookup ct <<< _.ids)
  traverse_ (\sid -> Modal.whenClose ev sid (close ct)) id

close ::
  forall m.
  ComponentType ->
  ComponentM m Unit
close ct = do
  H.modify_ \old -> old { ids = Data.Map.delete ct old.ids }

open ::
  forall m.
  MonadAff m =>
  ComponentType ->
  ComponentM m Unit
open ct = do
  id <- Modal.initializeWith (Just <<< HandleKey ct)
  H.modify_ \old -> old { ids = Data.Map.insert ct id old.ids }

toast ::
  forall m.
  ComponentM m Unit
toast = do
  H.modify_ _ { toast = true }

render ::
  forall m.
  MonadAff m =>
  State ->
  ComponentHTML m
render st =
  HH.div_
  [ Documentation.block_
    { header: "Modals"
    , subheader: "Forest's favorite UI implement"
    }
    [ Backdrop.backdrop
      [ css "flex items-center" ]
      [ Backdrop.content
        [ css "mt-0 text-center" ]
        [ Button.button
          [ HE.onClick \_ -> Open PanelLeft ]
          [ HH.text "Open Left Panel" ]
        ]
      ]
    , Backdrop.backdropDark
      [ css "flex items-center" ]
      [ Backdrop.content
        [ css "mt-0 text-center" ]
        [ Button.buttonPrimary
          [ HE.onClick \_ -> Open Modal ]
          [ HH.text "Open Modal" ]
        ]
      ]
    , Backdrop.backdrop
      [ css "flex items-center" ]
      [ Backdrop.content
        [ css "mt-0 text-center" ]
        [ Button.button
          [ HE.onClick \_ -> Open PanelRight ]
          [ HH.text "Open Right Panel" ]
        ]
      ]
    ]
  , if isJust (Data.Map.lookup Modal st.ids) then renderModal else HH.text ""
  , renderPanelLeft (isJust (Data.Map.lookup PanelLeft st.ids))
  , renderPanelRight (isJust (Data.Map.lookup PanelRight st.ids))
  , Ocelot.Block.Toast.toast
      [ Ocelot.Block.Toast.visible st.toast ]
      [ Icon.error
        [ css "text-red text-2xl mr-2" ]
      , HH.text "Failure"
      ]
  ]

renderModal ::
  forall m.
  MonadAff m =>
  ComponentHTML m
renderModal =
  Modal.modal_ (Close Modal)
  [ Modal.header
    { buttons:
      [ HH.a
        [ HP.classes ( Format.linkDarkClasses <> [ HH.ClassName "mr-4" ] )
        , HE.onClick \_ -> Close Modal ]
        [ HH.text "Cancel" ]
      , Button.buttonPrimary
        [ HE.onClick \_ -> Toast ]
        [ HH.text "Submit" ]
      ]
    , title: [ HH.text "Editing" ]
    }
  , Modal.body_
    [ renderContent "modal-1" ]
  ]

renderPanelLeft ::
  forall m.
  MonadAff m =>
  Boolean ->
  ComponentHTML m
renderPanelLeft visible =
  Panel.panelLeft_
  visible
  (Close PanelLeft)
  [ Panel.header
    { buttons:
      [ Button.buttonClear
        [ HE.onClick \_ -> Close PanelLeft ]
        [ Icon.close_ ]
      ]
    , title: [ HH.text "Editing" ]
    }
  , Panel.body_
    [ renderContent "left-panel-1"
    , renderContent "left-panel-2"
    , renderContent "left-panel-3"
    ]
  ]

renderPanelRight ::
  forall m.
  MonadAff m =>
  Boolean ->
  ComponentHTML m
renderPanelRight visible =
  Panel.panelRight_
  visible
  (Close PanelRight)
  [ Panel.header
    { buttons:
      [ Button.buttonClear
        [ HE.onClick \_ -> Close PanelRight ]
        [ Icon.close_ ]
      ]
    , title: [ HH.text "Editing" ]
    }
  , Panel.body_
    [ renderContent "right-panel-1"
    , renderContent "right-panel-2"
    , renderContent "right-panel-3"
    ]
  ]

renderContent ::
  forall m.
  MonadAff m =>
  String ->
  ComponentHTML m
renderContent label =
  Card.card
  [ HP.class_ $ HH.ClassName "m-10" ]
  [ HH.h3
    [ HP.classes Format.captionClasses ]
    [ HH.text "Standard" ]
  , FormField.field_
    { label: HH.text "Locations"
    , helpText: [ HH.text "Search your top destinations." ]
    , error: []
    , inputId: "locations"
    }
    [ HH.slot_ _cp1 label TA.multi
      ( TA.asyncMulti
        { renderFuzzy: HH.span_ <<< IC.boldMatches "name"
        , itemToObject: Async.locationToObject
        , async: Async.loadFromSource Async.locations
        }
        [ HP.placeholder "Search locations..."
        , HP.id_ "locations"
        ]
      )
    ]
  , HH.h3
    [ HP.classes Format.captionClasses ]
    [ HH.text "Standard Hydrated" ]
  , FormField.field_
    { label: HH.text "Users"
    , helpText: [ HH.text "Search your favorite companion." ]
    , error: []
    , inputId: "users"
    }
    [ HH.slot_ _cp2 label TA.multi
      ( TA.asyncMulti
        { renderFuzzy: Async.renderFuzzyUser
        , itemToObject: Async.userToObject
        , async: Async.loadFromSource Async.users
        }
        [ HP.placeholder "Search users..."
        , HP.id_ "users"
        ]
      )
    ]
  ]
