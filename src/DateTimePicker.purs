module Ocelot.DateTimePicker 
  ( Action
  , ChildSlots
  , Component
  , ComponentHTML
  , ComponentM
  , ComponentRender
  , Input
  , Interval
  , Output(..)
  , Query(..)
  , Slot
  , State
  , component
  ) where

import Prelude
import Data.DateTime (Date, DateTime(..), Month, Time, Year)
import Data.DateTime as Date.DateTime
import Data.Foldable as Data.Foldable
import Data.Maybe (Maybe(..))
import Data.Maybe as Data.Maybe
import Data.Tuple.Nested (type (/\))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Ocelot.DatePicker as DatePicker
import Ocelot.HTML.Properties (css)
import Ocelot.TimePicker as TimePicker
import Type.Data.Symbol (SProxy(..))

--------
-- Types

data Action
  = HandleDate DatePicker.Output
  | HandleTime TimePicker.Output
  | Receive Input

type ChildSlots =
  ( datepicker :: DatePicker.Slot Unit
  , timepicker :: TimePicker.Slot Unit
  )

type Component m = H.Component HH.HTML Query Input Output m

type ComponentHTML m = H.ComponentHTML Action ChildSlots m

type ComponentM m a = H.HalogenM State Action ChildSlots Output m a

type ComponentRender m = State -> ComponentHTML m

type Input =
  { disabled :: Boolean
  , interval :: Maybe Interval
  , selection :: Maybe DateTime
  , targetDate :: Maybe (Year /\ Month)
  }

type Interval =
  { start :: Maybe DateTime
  , end :: Maybe DateTime
  }

data Output
  = SelectionChanged (Maybe DateTime)
  | DateOutput DatePicker.Output
  | TimeOutput TimePicker.Output

data Query a
  = GetSelection (DateTime -> a)
  | SetSelection (Maybe DateTime) a
  | SendDateQuery (DatePicker.Query Unit) a
  | SendTimeQuery (TimePicker.Query Unit) a
  | SetDisabled Boolean a

type Slot = H.Slot Query Output

type State =
  { date :: Maybe Date
  , disabled :: Boolean
  , interval :: Maybe Interval
  , targetDate :: Maybe (Year /\ Month)
  , time :: Maybe Time
  }

------------
-- Component

component :: forall m. MonadAff m => Component m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , handleQuery = handleQuery
      , receive = Just <<< Receive
      }
  }

---------
-- Values

_datepicker = SProxy :: SProxy "datepicker"
_timepicker = SProxy :: SProxy "timepicker"

handleAction :: forall m. Action -> ComponentM m Unit
handleAction = case _ of
  HandleDate msg -> case msg of
    DatePicker.SelectionChanged date' -> do
      state <- H.get
      raiseSelectionChanged state.interval date' state.time
      H.modify_ _ { date = date' }
    _ -> H.raise $ DateOutput msg
  HandleTime msg -> case msg of
    TimePicker.SelectionChanged time' -> do
      state <- H.get
      raiseSelectionChanged state.interval state.date time'
      H.modify_ _ { time = time' }
    _ -> H.raise $ TimeOutput msg
  Receive input -> do
    H.modify_ _ { interval = input.interval }

handleQuery :: forall m a. Query a -> ComponentM m (Maybe a)
handleQuery = case _ of
  GetSelection reply -> do
    { time, date } <- H.get
    pure $ reply <$> (DateTime <$> date <*> time)
  SetDisabled disabled a -> Just a <$ do
    H.modify_ _ { disabled = disabled }
    void $ H.query _datepicker unit $ H.tell $ DatePicker.SetDisabled disabled
    void $ H.query _timepicker unit $ H.tell $ TimePicker.SetDisabled disabled
  SetSelection dateTime a -> Just a <$ do
    let date' = dateTime <#> Date.DateTime.date
        time' = dateTime <#> Date.DateTime.time
    void $ H.query _datepicker unit $ H.tell $ DatePicker.SetSelection date'
    void $ H.query _timepicker unit $ H.tell $ TimePicker.SetSelection time'
    H.modify_ _ { date = date', time = time' }
  SendDateQuery q a -> Just a <$ H.query _datepicker unit q
  SendTimeQuery q a -> Just a <$ H.query _timepicker unit q

initialState :: Input -> State
initialState input =
  { date: input.selection <#> Date.DateTime.date
  , disabled: input.disabled
  , interval: input.interval
  , targetDate: input.targetDate
  , time: input.selection <#> Date.DateTime.time
  }

-- check if a datetime is within a **closed** interval
isWithinInterval :: Interval -> DateTime -> Boolean
isWithinInterval interval x =
  Data.Foldable.and
    [ Data.Maybe.maybe true (_ <= x) interval.start
    , Data.Maybe.maybe true (x <= _) interval.end
    ]

raiseSelectionChanged ::
  forall m.
  Maybe Interval ->
  Maybe Date ->
  Maybe Time ->
  ComponentM m Unit
raiseSelectionChanged mInterval mDate mTime = case mInterval of
  Nothing -> H.raise $ SelectionChanged mDateTime
  Just interval -> case mDateTime of
    Nothing -> H.raise $ SelectionChanged mDateTime
    Just dateTime
      | isWithinInterval interval dateTime -> H.raise $ SelectionChanged mDateTime
      | otherwise -> pure unit -- NOTE transient state during parent-child synchronization
  where
  mDateTime :: Maybe DateTime
  mDateTime = DateTime <$> mDate <*> mTime


render :: forall m. MonadAff m => ComponentRender m
render state =
  HH.div
    [ css "flex" ]
    [ HH.div
      [ css "w-1/2 mr-2" ]
      [ HH.slot _datepicker unit DatePicker.component
        { disabled: state.disabled
        , interval: do
            interval <- state.interval
            pure
              { start: interval.start <#> Date.DateTime.date
              , end: interval.end <#> Date.DateTime.date
              }
        , selection: state.date
        , targetDate: state.targetDate
        }
        (Just <<< HandleDate)
      ]
    , HH.div
      [ css "flex-1" ]
      [ HH.slot _timepicker unit TimePicker.component
        { disabled: state.disabled
        , interval: do
            interval <- state.interval
            pure
              { start:
                  if (interval.start <#> Date.DateTime.date) == state.date then
                    interval.start <#> Date.DateTime.time
                  else
                    Nothing
              , end:
                  if (interval.end <#> Date.DateTime.date) == state.date then
                    interval.end <#> Date.DateTime.time
                  else
                    Nothing
              }
        , selection: state.time
        }
        (Just <<< HandleTime)
      ]
    ]
