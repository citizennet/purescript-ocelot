module Ocelot.Component.DateTimePicker where

import Prelude

import Data.DateTime (Date, DateTime(..), Month, Time, Year, date, time)
import Data.Either.Nested (Either2)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Ocelot.Component.DatePicker as DP
import Ocelot.Component.TimePicker as TP
import Ocelot.HTML.Properties (css)

type State =
  { date :: Maybe Date
  , time :: Maybe Time
  , targetDate :: Maybe (Tuple Year Month)
  , disabled :: Boolean
  }

type Input =
  { selection :: Maybe DateTime
  , targetDate :: Maybe (Tuple Year Month)
  , disabled :: Boolean
  }

data Query a
  = HandleDate DP.Message a
  | HandleTime TP.Message a
  | GetSelection (Maybe DateTime -> a)
  | SetSelection (Maybe DateTime) a
  | SendDateQuery (DP.Query Unit) a
  | SendTimeQuery (TP.Query Unit) a

data Message
  = SelectionChanged (Maybe DateTime)
  | DateMessage DP.Message
  | TimeMessage TP.Message

type ParentHTML m = H.ParentHTML Query ChildQuery Input m

-- type ChildSlot = Either2 Unit Unit
type ChildSlots =
  ( a :: H.Slot DP.Query Void Unit
  , b :: H.Slot TP.Query Void Unit
  )

_a = SProxy :: SProxy "a"
_b = SProxy :: SProxy "b"

type ChildQuery = Coproduct2 DP.Query TP.Query

component :: ∀ m. MonadAff m => H.Component HH.HTML Query Input Message m
component =
  H.lifecycleParentComponent
    { initialState
    , render
    , eval
    , receiver: const Nothing
    , initializer: Nothing
    , finalizer: Nothing
    }
  where
    initialState :: Input -> State
    initialState { selection, targetDate, disabled } =
      { date: date <$> selection
      , time: time <$> selection
      , targetDate
      , disabled
      }

    eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Message m
    eval = case _ of
      HandleDate msg a -> a <$ case msg of
        DP.SelectionChanged date' -> do
          time' <- H.gets _.time
          H.raise $ SelectionChanged (DateTime <$> date' <*> time')
          H.modify_ _ { date = date' }

        _ -> H.raise $ DateMessage msg

      HandleTime msg a -> a <$ case msg of
        TP.SelectionChanged time' -> do
          date' <- H.gets _.date
          H.raise $ SelectionChanged (DateTime <$> date' <*> time')
          H.modify_ _ { time = time' }

        _ -> H.raise $ TimeMessage msg

      GetSelection reply -> do
        { time, date } <- H.get
        pure $ reply (DateTime <$> date <*> time)

      SetSelection dateTime a -> a <$ do
        let date' = date <$> dateTime
            time' = time <$> dateTime
        void $ H.query _a unit $ DP.SetSelection date' a
        void $ H.query _b unit $ TP.SetSelection time' a
        H.modify_ _ { date = date', time = time' }

      SendDateQuery q a -> a <$ H.query _a unit q

      SendTimeQuery q a -> a <$ H.query _b unit q


    render :: State -> H.ParentHTML Query ChildQuery ChildSlot m
    render { date, time, targetDate, disabled } =
      HH.div
        [ css "flex" ]
        [ HH.div
          [ css "w-1/2 mr-2" ]
          [ HH.slot _a unit DP.component
            { targetDate
            , selection: date
            , disabled
            }
            (HE.input HandleDate)
          ]
        , HH.div
          [ css "flex-1" ]
          [ HH.slot _b unit TP.component
            { selection: time
            , disabled
            }
            (HE.input HandleTime)
          ]
        ]
