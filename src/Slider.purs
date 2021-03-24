-- | * track = background track + interval(s)
-- |   * thumb: draggable node on track
-- |   * mark: static node on track
-- | * axis
module Ocelot.Slider
  ( Interval(..)
  , Output
  , Query(..)
  , Slot
  , component
  ) where

import Prelude

import Data.Array as Data.Array
import Data.Int as Data.Int
import Data.Maybe (Maybe(..))
import Data.Monoid as Data.Monoid
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console as Effect.Class.Console
import Halogen as Halogen
import Halogen.HTML as Halogen.HTML
import Halogen.HTML.Events as Halogen.HTML.Events
import Halogen.Query.EventSource as Halogen.Query.EventSource
import Halogen.Svg.Attributes as Halogen.Svg.Attributes
import Ocelot.Slider.Render as Ocelot.Slider.Render
import Web.Event.Event as Web.Event.Event
import Web.HTML as Web.HTML
import Web.HTML.Window as Web.HTML.Window
import Web.UIEvent.MouseEvent as Web.UIEvent.MouseEvent
import Web.UIEvent.MouseEvent.EventTypes as Web.UIEvent.MouseEvent.EventTypes

type Slot = Halogen.Slot Query Output

type Component m = Halogen.Component Halogen.HTML.HTML Query Input Output m
type ComponentHTML m = Halogen.ComponentHTML Action ChildSlots m
type ComponentM m a = Halogen.HalogenM State Action ChildSlots Output m a

type State =
  { input :: Input
  , thumbs :: ThumbStatus
  }

data ThumbStatus
  = Idle IdleState
  | Editing EditingState

type IdleState =
  Array { percent :: Number }

type EditingState =
  { start ::
      { positionX :: { px :: Number }
      , value :: { percent :: Number }
      }
  , static :: Array { percent :: Number }
  , moving :: { percent :: Number }
  , subscriptions ::
      { onMouseMove :: Halogen.SubscriptionId
      , onMouseUp :: Halogen.SubscriptionId
      }
  }

getThumbs :: ThumbStatus -> Array { percent :: Number }
getThumbs = case _ of
  Idle xs -> xs
  Editing { static, moving } -> Data.Array.sort (static <> [ moving ])

data Action
  = MouseDownOnThumb Int Web.UIEvent.MouseEvent.MouseEvent
  | MouseMoveWithThumb Web.UIEvent.MouseEvent.MouseEvent
  | MouseUpFromThumb Web.UIEvent.MouseEvent.MouseEvent

data Query a
  = SetThumbCount Int a

-- | * axis: a list of labels positioned under the track
-- | * layout: see comments in Ocelot.Slider.Render
-- | * marks
-- |   * Nothing: thumbs can slide continuously
-- |   * Just: thumbs can only sit on discrete positions specified by marks
-- | * minDistance:
-- |   * Nothing: allow overlapping thumbs
-- |   * Just: minimal distance between any pair of thumbs
-- | * renderIntervals: customized render for intervals between thumbs
type Input =
  { axis :: Maybe (Array { label :: String, percent :: Number })
  , layout :: Ocelot.Slider.Render.Config
  , marks :: Maybe (Array { percent :: Number })
  , minDistance :: Maybe { percent :: Number }
  , renderIntervals ::
      Array Interval ->
      Array Halogen.HTML.PlainHTML
  }

data Interval
  = StartToThumb { start :: { percent :: Number }, thumb :: { percent :: Number } }
  | BetweenThumbs { left :: { percent :: Number }, right :: { percent :: Number } }
  | ThumbToEnd { thumb :: { percent :: Number }, end :: { percent :: Number } }

data Output
  = ValueChanged (Array { percent :: Number })

type ChildSlots =
  ()

component ::
  forall m.
  MonadAff m =>
  Component m
component =
  Halogen.mkComponent
    { initialState
    , render
    , eval:
        Halogen.mkEval
          Halogen.defaultEval
            { handleAction = handleAction
            }
    }

initialState :: Input -> State
initialState input =
  { input
  , thumbs: Idle [ { percent: 0.0 }, { percent: 50.0 } ] -- TODO AS-1146 leave one at 0.0 to start
  }

handleAction ::
  forall m.
  MonadAff m =>
  Action ->
  ComponentM m Unit
handleAction = case _ of
  MouseDownOnThumb index mouseEvent -> do
    pauseEvent mouseEvent
    Effect.Class.Console.log $ "MouseDown: " <> show index
    state <- Halogen.get
    case state.thumbs of
      Editing _ -> pure unit
      Idle idleState -> handleMouseDownOnThumb index mouseEvent idleState
  MouseMoveWithThumb mouseEvent -> do
    pauseEvent mouseEvent
    state <- Halogen.get
    case state.thumbs of
      Idle _ -> pure unit
      Editing editingState -> do
        handleMouseMoveWithThumb mouseEvent state editingState
  MouseUpFromThumb mouseEvent -> do
    pauseEvent mouseEvent
    Effect.Class.Console.log $ "MouseUp"
    state <- Halogen.get
    case state.thumbs of
      Idle _ -> pure unit
      Editing editingState -> handleMouseUpFromThumb mouseEvent editingState

handleMouseDownOnThumb ::
  forall m.
  MonadAff m =>
  Int ->
  Web.UIEvent.MouseEvent.MouseEvent ->
  IdleState ->
  ComponentM m Unit
handleMouseDownOnThumb index mouseEvent thumbs = case unsnocAt index thumbs of
  Nothing -> pure unit
  Just { item, rest } -> do
    subscriptions <- listenAll
    Halogen.modify_ _
      { thumbs =
          Editing
            { start:
              { positionX: getPositionX mouseEvent
              , value: item
              }
            , static: rest
            , moving: item
            , subscriptions
            }
      }

handleMouseMoveWithThumb ::
  forall m.
  MonadAff m =>
  Web.UIEvent.MouseEvent.MouseEvent ->
  State ->
  EditingState ->
  ComponentM m Unit
handleMouseMoveWithThumb mouseEvent state old@{ start } = do
  let
    endPositionX :: { px :: Number }
    endPositionX = getPositionX mouseEvent

    diff :: { percent :: Number }
    diff =
      Ocelot.Slider.Render.pixelToPercent state.input.layout
        (endPositionX - start.positionX)

    end :: { percent :: Number }
    end = start.value + diff

    calibrated :: { percent :: Number }
    calibrated = calibrateValue state { start: start.value, end }

  Effect.Class.Console.log $ "MouseMove: " <> show diff.percent <> "%"
  Halogen.modify_ _
    { thumbs = Editing old { moving = calibrated } }

handleMouseUpFromThumb ::
  forall m.
  Web.UIEvent.MouseEvent.MouseEvent ->
  EditingState ->
  ComponentM m Unit
handleMouseUpFromThumb mouseEvent { static, moving, subscriptions } = do
  muteAllListeners subscriptions
  Halogen.modify_ \old ->
    old { thumbs = Idle (Data.Array.sort (static <> [ moving ]))}

getPositionX :: Web.UIEvent.MouseEvent.MouseEvent -> { px :: Number }
getPositionX mouseEvent =
  { px: _ } <<< Data.Int.toNumber
    $ Web.UIEvent.MouseEvent.pageX mouseEvent

calibrateValue ::
  State ->
  { start :: { percent :: Number }
  , end :: { percent :: Number }
  } ->
  { percent :: Number }
calibrateValue state { start, end } = trimBoundary end

trimBoundary ::
  { percent :: Number } ->
  { percent :: Number }
trimBoundary x = clamp boundary.start boundary.end x

listenAll ::
  forall m.
  MonadAff m =>
  ComponentM m
    { onMouseMove :: Halogen.SubscriptionId
    , onMouseUp :: Halogen.SubscriptionId
    }
listenAll = do
  window <- Halogen.liftEffect Web.HTML.window
  onMouseMove <- listenOnMouseMove window
  onMouseUp <- listenOnMouseUp window
  pure { onMouseMove, onMouseUp }

listenOnMouseMove ::
  forall m.
  MonadAff m =>
  Web.HTML.Window.Window ->
  ComponentM m Halogen.SubscriptionId
listenOnMouseMove window = do
  Halogen.subscribe
    $ Halogen.Query.EventSource.eventListenerEventSource
        Web.UIEvent.MouseEvent.EventTypes.mousemove
        (Web.HTML.Window.toEventTarget window)
        toAction
  where
  toAction :: Web.Event.Event.Event -> Maybe Action
  toAction = map MouseMoveWithThumb <<< Web.UIEvent.MouseEvent.fromEvent

listenOnMouseUp ::
  forall m.
  MonadAff m =>
  Web.HTML.Window.Window ->
  ComponentM m Halogen.SubscriptionId
listenOnMouseUp window = do
  Halogen.subscribe
    $ Halogen.Query.EventSource.eventListenerEventSource
        Web.UIEvent.MouseEvent.EventTypes.mouseup
        (Web.HTML.Window.toEventTarget window)
        toAction
  where
  toAction :: Web.Event.Event.Event -> Maybe Action
  toAction = map MouseUpFromThumb <<< Web.UIEvent.MouseEvent.fromEvent

muteAllListeners ::
  forall m.
  { onMouseMove :: Halogen.SubscriptionId
  , onMouseUp :: Halogen.SubscriptionId
  } ->
  ComponentM m Unit
muteAllListeners subscriptions = do
  Halogen.unsubscribe subscriptions.onMouseMove
  Halogen.unsubscribe subscriptions.onMouseUp

pauseEvent ::
  forall m.
  MonadAff m =>
  Web.UIEvent.MouseEvent.MouseEvent ->
  ComponentM m Unit
pauseEvent mouseEvent = do
  let
    event :: Web.Event.Event.Event
    event = Web.UIEvent.MouseEvent.toEvent mouseEvent
  Halogen.liftEffect
    $ Web.Event.Event.stopPropagation event
  Halogen.liftEffect
    $ Web.Event.Event.preventDefault event

render :: forall m. State -> ComponentHTML m
render state =
  Ocelot.Slider.Render.frame state.input.layout []
    [ renderTrack state
    , renderAxis state
    , renderThumbs state
    ]

renderAxis :: forall m. State -> ComponentHTML m
renderAxis state = case state.input.axis of
  Nothing -> Halogen.HTML.text ""
  Just axisData ->
    Ocelot.Slider.Render.axisContainer state.input.layout
      (Ocelot.Slider.Render.axis state.input.layout axisData)

renderMarks :: forall m. State -> ComponentHTML m
renderMarks state = case state.input.marks of
  Nothing -> Halogen.HTML.text ""
  Just marks ->
    Ocelot.Slider.Render.markContainer state.input.layout
      (renderMark state <$> marks)

renderMark :: forall m. State -> { percent :: Number } -> ComponentHTML m
renderMark state percent =
  Ocelot.Slider.Render.mark state.input.layout percent
    []

renderThumbs :: forall m. State -> ComponentHTML m
renderThumbs state =
  Ocelot.Slider.Render.thumbContainer state.input.layout
    (Data.Array.mapWithIndex (renderThumb state) (getThumbs state.thumbs))

renderThumb :: forall m. State -> Int -> { percent :: Number } -> ComponentHTML m
renderThumb state index percent =
  Ocelot.Slider.Render.thumb state.input.layout percent
    [ Halogen.HTML.Events.onMouseDown  (Just <<< MouseDownOnThumb index)
    ]

renderTrack :: forall m. State -> ComponentHTML m
renderTrack state =
  Ocelot.Slider.Render.trackContainer state.input.layout
    ( [ Ocelot.Slider.Render.track state.input.layout
        [ Halogen.Svg.Attributes.fill
            (Just (Halogen.Svg.Attributes.RGB 229 229 229))
        ]
      , renderMarks state
      ]
        <> renderIntervals state
    )

renderIntervals :: forall m. State -> Array (ComponentHTML m)
renderIntervals state =
  map Halogen.HTML.fromPlainHTML
    <<< state.input.renderIntervals
    $ getIntervals (getThumbs state.thumbs)
  where
  getIntervals ::
    Array { percent :: Number } ->
    Array Interval
  getIntervals xs = case Data.Array.uncons xs of
    Nothing -> []
    Just { head: firstThumb, tail: xs1 } -> do
      case Data.Array.unsnoc xs1 of
        Nothing ->
          [ Data.Monoid.guard (boundary.start /= firstThumb)
              [ StartToThumb { start: boundary.start, thumb: firstThumb } ]
          , Data.Monoid.guard (firstThumb /= boundary.end)
              [ ThumbToEnd { thumb: firstThumb, end: boundary.end } ]
          ]
            # join
        Just { init: middle, last: lastThumb } ->
          [ Data.Monoid.guard (boundary.start /= firstThumb)
              [ StartToThumb { start: boundary.start, thumb: firstThumb } ]
          , betweenThumbs firstThumb middle lastThumb
          , Data.Monoid.guard (lastThumb /= boundary.end)
              [ ThumbToEnd { thumb: lastThumb, end: boundary.end } ]
          ]
            # join

  betweenThumbs ::
    { percent :: Number } ->
    Array { percent :: Number } ->
    { percent :: Number } ->
    Array Interval
  betweenThumbs first middle last =
    Data.Array.zipWith
      (\left right -> BetweenThumbs { left, right })
      ([ first ] <> middle)
      (middle <> [ last ])

unsnocAt :: forall a. Int -> Array a -> Maybe { item :: a , rest :: Array a }
unsnocAt index xs = do
  item <- Data.Array.index xs index
  rest <- Data.Array.deleteAt index xs
  pure { item, rest }

---------------
-- Constants --
---------------
boundary :: { start :: { percent :: Number }, end :: { percent :: Number } }
boundary = { start: { percent: 0.0 }, end: { percent: 100.0 } }
