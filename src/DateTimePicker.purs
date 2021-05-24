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
import Data.DateTime (Date, DateTime(..), Month, Time, Year, date, time)
import Data.Maybe (Maybe(..))
import Data.Maybe as Data.Maybe
import Data.Tuple.Nested (type (/\))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Ocelot.DatePicker as DatePicker
import Ocelot.HTML.Properties (css)
import Ocelot.TimePicker as TimePicker
import Type.Proxy (Proxy(..))

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

type Component m = H.Component Query Input Output m

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

_datepicker = Proxy :: Proxy "datepicker"
_timepicker = Proxy :: Proxy "timepicker"

handleAction :: forall m. Action -> ComponentM m Unit
handleAction = case _ of
  HandleDate msg -> case msg of
    DatePicker.SelectionChanged date' -> do
      time' <- H.gets _.time
      H.raise $ SelectionChanged (DateTime <$> date' <*> time')
      H.modify_ _ { date = date' }
    _ -> H.raise $ DateOutput msg
  HandleTime msg -> case msg of
    TimePicker.SelectionChanged time' -> do
      date' <- H.gets _.date
      H.raise $ SelectionChanged (DateTime <$> date' <*> time')
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
    void $ H.tell _datepicker unit $ DatePicker.SetDisabled disabled
    void $ H.tell _timepicker unit $ TimePicker.SetDisabled disabled
  SetSelection dateTime a -> Just a <$ do
    let date' = date <$> dateTime
        time' = time <$> dateTime
    void $ H.tell _datepicker unit $ DatePicker.SetSelection date'
    void $ H.tell _timepicker unit $ TimePicker.SetSelection time'
    H.modify_ _ { date = date', time = time' }
  SendDateQuery q a -> Just a <$ H.query _datepicker unit q
  SendTimeQuery q a -> Just a <$ H.query _timepicker unit q

initialState :: Input -> State
initialState input =
  { date: date <$> input.selection
  , disabled: input.disabled
  , interval: input.interval
  , targetDate: input.targetDate
  , time: time <$> input.selection
  }

render :: forall m. MonadAff m => ComponentRender m
render { date, time, targetDate, disabled, interval } =
  HH.div
    [ css "flex" ]
    [ HH.div
      [ css "w-1/2 mr-2" ]
      [ HH.slot _datepicker unit DatePicker.component
        { disabled
        , interval: Nothing -- TODO AS-1344
        , selection: date
        , targetDate
        }
        HandleDate
      ]
    , HH.div
      [ css "flex-1" ]
      [ HH.slot _timepicker unit TimePicker.component
        { disabled:
            disabled
              || (Data.Maybe.isJust interval && Data.Maybe.isNothing date)
        , interval: Nothing -- TODO AS-1344
        , selection: time
        }
        HandleTime
      ]
    ]
