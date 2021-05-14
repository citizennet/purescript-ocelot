-- | * track = background track + interval(s)
-- |   * thumb: draggable node on track
-- |   * mark: static node on track
-- | * axis
module Ocelot.Slider
  ( Interval(..)
  , Output(..)
  , Query(..)
  , Slot
  , component
  ) where

import Prelude

import Control.Monad.Rec.Class as Control.Monad.Rec.Class
import Data.Array as Data.Array
import Data.Int as Data.Int
import Data.Map as Data.Map
import Data.Maybe (Maybe(..))
import Data.Maybe as Data.Maybe
import Data.Monoid as Data.Monoid
import Data.Ord as Data.Ord
import Effect.Aff.Class (class MonadAff)
import Halogen as Halogen
import Halogen.HTML as Halogen.HTML
import Halogen.HTML.Events as Halogen.HTML.Events
import Halogen.Query.Event as Halogen.Query.Event
import Halogen.Svg.Attributes as Halogen.Svg.Attributes
import Ocelot.Data.IntervalTree as Ocelot.Data.IntervalTree
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
  = ReplaceThumbs (Array { percent :: Number }) a
  | SetDisabled Boolean a
  | SetThumbCount Int a

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
  , disabled :: Boolean
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
            , handleQuery = handleQuery
            }
    }

initialState :: Input -> State
initialState input =
  { input
  , thumbs: Idle [ { percent: 0.0 } ]
  }

handleAction ::
  forall m.
  MonadAff m =>
  Action ->
  ComponentM m Unit
handleAction = case _ of
  MouseDownOnThumb index mouseEvent -> do
    pauseEvent mouseEvent
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
    state <- Halogen.get
    case state.thumbs of
      Idle _ -> pure unit
      Editing editingState -> handleMouseUpFromThumb mouseEvent editingState

handleQuery ::
  forall a m.
  Query a ->
  ComponentM m (Maybe a)
handleQuery = case _ of
  ReplaceThumbs thumbs a -> do
    state <- Halogen.get
    case state.thumbs of
      Idle _ -> pure unit
      Editing { subscriptions } -> muteAllListeners subscriptions
    Halogen.modify_ _ { thumbs = Idle thumbs }
    pure (Just a)
  SetDisabled disabled a -> do
    Halogen.modify_ _ { input { disabled = disabled } }
    pure (Just a)
  SetThumbCount n a
    | n < 1 -> pure Nothing
    | otherwise -> do
      state <- Halogen.get
      case state.thumbs of
        Idle idleState -> handleSetThumbCount a n state.input idleState
        Editing _ -> pure Nothing

handleSetThumbCount ::
  forall a m.
  a ->
  Int ->
  Input ->
  IdleState ->
  ComponentM m (Maybe a)
handleSetThumbCount a n input thumbs = case n - Data.Array.length thumbs of
  diff
    | diff < 0 -> removeExtraThumbs a n thumbs
    | diff > 0 -> addNewThumbs a input diff thumbs
    | otherwise -> pure (Just a)

removeExtraThumbs ::
  forall a m.
  a ->
  Int ->
  Array { percent :: Number } ->
  ComponentM m (Maybe a)
removeExtraThumbs a n thumbs = do
  let
    new :: Array { percent :: Number }
    new = Data.Array.take n thumbs
  Halogen.modify_ _ { thumbs = Idle new }
  Halogen.raise (ValueChanged new)
  pure (Just a)

addNewThumbs ::
  forall a m.
  a ->
  Input ->
  Int ->
  Array { percent :: Number } ->
  ComponentM m (Maybe a)
addNewThumbs a input diff thumbs = case input.minDistance of
  Nothing -> do
    let
      new :: Array { percent :: Number }
      new = (Data.Array.range 1 diff <#> \_ -> boundary.start) <> thumbs
    Halogen.modify_ _ { thumbs = Idle new }
    Halogen.raise (ValueChanged new)
    pure (Just a)
  Just minDistance -> do
    let
      neighborTree :: Ocelot.Data.IntervalTree.IntervalTree { percent :: Number }
      neighborTree =
        Ocelot.Data.IntervalTree.fromIntervals
          $ getNeighbors minDistance thumbs

      newThumbs :: Array { percent :: Number }
      newThumbs = case input.marks of
        Nothing -> newThumbsContinuous minDistance diff neighborTree
        Just marks -> newThumbsDiscrete minDistance diff neighborTree marks
      new :: Array { percent :: Number }
      new = Data.Array.sort (thumbs <> newThumbs)
    if Data.Array.length newThumbs == diff then do
      Halogen.modify_ _ { thumbs = Idle new }
      Halogen.raise (ValueChanged new)
      pure (Just a)
    else
      pure Nothing

newThumbsContinuous ::
  { percent :: Number } ->
  Int ->
  Ocelot.Data.IntervalTree.IntervalTree { percent :: Number } ->
  Array { percent :: Number }
newThumbsContinuous minDistance diff neighborTree =
  Control.Monad.Rec.Class.tailRec go
    { minDistance
    , n: diff
    , neighborTree
    , newThumbs: []
    }
  where
  go :: _ -> Control.Monad.Rec.Class.Step _ (Array { percent :: Number })
  go x = case x.n of
    0 -> Control.Monad.Rec.Class.Done x.newThumbs
    _ ->
      let
        surrounding ::
          { left :: Maybe { key :: { percent :: Number }, value :: Ocelot.Data.IntervalTree.IntervalPoint }
          , right :: Maybe { key :: { percent :: Number }, value :: Ocelot.Data.IntervalTree.IntervalPoint }
          }
        surrounding = Ocelot.Data.IntervalTree.lookupInterval boundary.start x.neighborTree

        newThumb :: { percent :: Number }
        newThumb = case surrounding.right of
          Nothing -> boundary.start
          Just right -> case right.value of
            Ocelot.Data.IntervalTree.StartPoint -> boundary.start
            Ocelot.Data.IntervalTree.EndPoint -> right.key
      in
        Control.Monad.Rec.Class.Loop
          { minDistance: x.minDistance
          , n: x.n - 1
          , neighborTree:
              Ocelot.Data.IntervalTree.insertInterval
                x.neighborTree
                { start: newThumb - x.minDistance, end: newThumb + x.minDistance }
          , newThumbs: Data.Array.snoc x.newThumbs newThumb
          }

newThumbsDiscrete ::
  { percent :: Number } ->
  Int ->
  Ocelot.Data.IntervalTree.IntervalTree { percent :: Number } ->
  Array { percent :: Number } ->
  Array { percent :: Number }
newThumbsDiscrete minDistance diff neighborTree marks =
  Control.Monad.Rec.Class.tailRec go
    { minDistance
    , n: diff
    , neighborTree
    , marks
    , newThumbs: []
    }
  where
  go :: _ -> Control.Monad.Rec.Class.Step _ (Array { percent :: Number })
  go x = case x.n of
    0 -> Control.Monad.Rec.Class.Done x.newThumbs
    _ ->
      let
        availablePositions :: Array { percent :: Number }
        availablePositions = getAvailablePositions x.neighborTree x.marks
      in case Data.Array.uncons availablePositions of
        Nothing -> Control.Monad.Rec.Class.Done x.newThumbs
        Just { head, tail } ->
          Control.Monad.Rec.Class.Loop
            { minDistance: x.minDistance
            , n: x.n - 1
            , neighborTree:
                Ocelot.Data.IntervalTree.insertInterval
                  x.neighborTree
                  { start: head - x.minDistance, end: head + x.minDistance }
            , marks: tail
            , newThumbs: Data.Array.snoc x.newThumbs head
            }

getAvailablePositions ::
  Ocelot.Data.IntervalTree.IntervalTree { percent :: Number } ->
  Array { percent :: Number } ->
  Array { percent :: Number }
getAvailablePositions neighborTree marks = Data.Array.filter filter marks
  where
  filter :: { percent :: Number } -> Boolean
  filter mark =
    let
      surrounding ::
        { left :: Maybe { key :: { percent :: Number }, value :: Ocelot.Data.IntervalTree.IntervalPoint }
        , right :: Maybe { key :: { percent :: Number }, value :: Ocelot.Data.IntervalTree.IntervalPoint }
        }
      surrounding = Ocelot.Data.IntervalTree.lookupInterval mark neighborTree
    in case surrounding.left, surrounding.right of
      Just left, Just right -> case left.value, right.value of
        Ocelot.Data.IntervalTree.StartPoint, Ocelot.Data.IntervalTree.EndPoint
          | mark /= left.key && mark /= right.key -> false
        _, _ -> true
      _, _ -> true

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
handleMouseMoveWithThumb mouseEvent state old@{ start, static } = do
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
    calibrated = calibrateValue state static { start: start.value, end }

  Halogen.modify_ _
    { thumbs = Editing old { moving = calibrated } }

handleMouseUpFromThumb ::
  forall m.
  Web.UIEvent.MouseEvent.MouseEvent ->
  EditingState ->
  ComponentM m Unit
handleMouseUpFromThumb mouseEvent { static, moving, subscriptions } = do
  muteAllListeners subscriptions
  let
    new :: Array { percent :: Number }
    new = Data.Array.sort (static <> [ moving ])
  Halogen.modify_ _ { thumbs = Idle new }
  Halogen.raise (ValueChanged new)

getPositionX :: Web.UIEvent.MouseEvent.MouseEvent -> { px :: Number }
getPositionX mouseEvent =
  { px: _ } <<< Data.Int.toNumber
    $ Web.UIEvent.MouseEvent.pageX mouseEvent

calibrateValue ::
  State ->
  Array { percent :: Number } ->
  { start :: { percent :: Number }
  , end :: { percent :: Number }
  } ->
  { percent :: Number }
calibrateValue state static { start, end } = case state.input.marks of
  Nothing ->
    trimNeighbor state.input.minDistance { start, static }
      <<< trimBoundary
      $ end
  Just marks ->
    alignToMarks state.input.minDistance { marks, start, static }
      <<< trimBoundary
      $ end

trimNeighbor ::
  Maybe { percent :: Number } ->
  { start :: { percent :: Number }
  , static :: Array { percent :: Number }
  } ->
  { percent :: Number } ->
  { percent :: Number }
trimNeighbor mMinDistance { start, static } x = case mMinDistance of
  Nothing -> x
  Just minDistance ->
    let
      surrounding ::
        { left :: Maybe { key :: { percent :: Number }, value :: Ocelot.Data.IntervalTree.IntervalPoint }
        , right :: Maybe { key :: { percent :: Number }, value :: Ocelot.Data.IntervalTree.IntervalPoint }
        }
      surrounding =
        Ocelot.Data.IntervalTree.lookupInterval x
          <<< Ocelot.Data.IntervalTree.fromIntervals
          <<< getNeighbors minDistance
          $ static
    in case surrounding.left, surrounding.right of
      Just left, Just right -> case left.value, right.value of
        Ocelot.Data.IntervalTree.StartPoint, Ocelot.Data.IntervalTree.EndPoint
         | absDistance x left.key <= absDistance x right.key
            && isWithinBoundary left.key -> left.key
         | absDistance x left.key <= absDistance x right.key
            && not isWithinBoundary left.key
            && isWithinBoundary right.key -> right.key
         | absDistance x left.key >= absDistance x right.key
            && isWithinBoundary right.key -> right.key
         | absDistance x left.key >= absDistance x right.key
            && not isWithinBoundary right.key
            && isWithinBoundary left.key -> left.key
         | otherwise -> start
        Ocelot.Data.IntervalTree.EndPoint, Ocelot.Data.IntervalTree.StartPoint -> x
        _, _ -> x
      _, _ -> x

getNeighbors ::
  { percent :: Number } ->
  Array { percent :: Number } ->
  Array { start :: { percent :: Number }, end :: { percent :: Number } }
getNeighbors minDistance thumbs = thumbs <#> \thumb ->
  { start: thumb - minDistance
  , end: thumb + minDistance
  }

alignToMarks ::
  Maybe { percent :: Number } ->
  { marks :: Array { percent :: Number }
  , start :: { percent :: Number }
  , static :: Array { percent :: Number }
  } ->
  { percent :: Number } ->
  { percent :: Number }
alignToMarks mMinDistance { marks, start, static } x
  | marks == [] = x
  | otherwise =
    Data.Maybe.fromMaybe start
      $ findClosest x filteredByMinDistance
  where
  filteredByMinDistance :: Array { percent :: Number }
  filteredByMinDistance = case mMinDistance of
    Nothing -> marks
    Just minDistance -> filterByMinDistance { minDistance, static } marks

filterByMinDistance ::
  { minDistance :: { percent :: Number }
  , static :: Array { percent :: Number }
  } ->
  Array { percent :: Number } ->
  Array { percent :: Number }
filterByMinDistance { minDistance, static } marks =
  Data.Array.foldl (filterOutNeighbor { minDistance }) marks static

filterOutNeighbor ::
  { minDistance :: { percent :: Number } } ->
  Array { percent :: Number } ->
  { percent :: Number } ->
  Array { percent :: Number }
filterOutNeighbor { minDistance } marks thumb = Data.Array.filter filter marks
  where
  filter :: { percent :: Number } -> Boolean
  filter mark = absDistance thumb mark >= minDistance

findClosest ::
  { percent :: Number } ->
  Array { percent :: Number } ->
  Maybe { percent :: Number }
findClosest x = map _.value <<< Data.Map.findMin <<< sortByDistance x

sortByDistance ::
  { percent :: Number } ->
  Array { percent :: Number } ->
  Data.Map.Map { percent :: Number } {- distance -}
    { percent :: Number } {- mark -}
sortByDistance x = Data.Array.foldMap reducer
  where
  reducer ::
    { percent :: Number } ->
    Data.Map.Map { percent :: Number }
      { percent :: Number }
  reducer mark = Data.Map.singleton (absDistance mark x) mark

absDistance :: { percent :: Number } -> { percent :: Number } -> { percent :: Number }
absDistance x y = Data.Ord.abs (x - y)

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
    $ Halogen.Query.Event.eventListener
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
    $ Halogen.Query.Event.eventListener
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
    [ Halogen.HTML.Events.onMouseDown
        if state.input.disabled
        then const Nothing
        else (Just <<< MouseDownOnThumb index)
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

isWithinBoundary :: { percent :: Number } -> Boolean
isWithinBoundary x = boundary.start <= x && x <= boundary.end
