module Ocelot.DatePicker
  ( Action
  , Aligned
  , CalendarItem
  , ChildSlots
  , Component
  , ComponentHTML
  , ComponentM
  , ComponentRender
  , CompositeAction
  , CompositeComponent
  , CompositeComponentHTML
  , CompositeComponentM
  , CompositeComponentRender
  , CompositeInput
  , CompositeQuery
  , CompositeState
  , Direction
  , EmbeddedAction(..)
  , EmbeddedChildSlots
  , Input
  , Interval
  , Output(..)
  , Query(..)
  , Slot
  , State
  , StateRow
  , component
  ) where

import Prelude
import Data.Array ((!!), drop, find, mapWithIndex, reverse, sort, take)
import Data.Array as Array
import Data.Date (Date, Month, Weekday(..), Year, canonicalDate, day, month, weekday, year)
import Data.DateTime.Instant (fromDate, toDateTime)
import Data.Either (either)
import Data.Enum (fromEnum)
import Data.Foldable as Data.Foldable
import Data.Formatter.DateTime (formatDateTime)
import Data.Fuzzy (Fuzzy(..))
import Data.Fuzzy as Data.Fuzzy
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Maybe as Data.Maybe
import Data.Rational ((%))
import Data.String (trim)
import Data.String.Regex (parseFlags, regex, replace)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff.Class (class MonadAff)
import Effect.Now (nowDate)
import Foreign.Object (Object, fromFoldable)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Ocelot.Block.Button as Button
import Ocelot.Block.Format as Format
import Ocelot.Block.Icon as Icon
import Ocelot.Block.Input as Input
import Ocelot.Block.Layout as Layout
import Ocelot.Data.DateTime (adjustDaysBy, dateRange, firstDateOfMonth, lastDateOfMonth, nextDay, nextYear, prevDay, yearsForward)
import Ocelot.Data.DateTime as ODT
import Ocelot.HTML.Properties (css)
import Select as S
import Select.Setters as SS
import Type.Data.Symbol (SProxy(..))
import Web.Event.Event (preventDefault)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE

--------
-- Types

-- NOTE overhead of component abstraction, need an action to 
-- re-raise output messages from the embedded component
data Action
  = PassingOutput Output
  | PassingReceive Input

-- A type to help assist making grid-based calendar layouts. Calendars
-- can use dates directly or use array lengths as offsets.
type Aligned =
  { pre  :: Array Date   -- Dates before the first of the month
  , body :: Array Date   -- Dates within the month
  , post :: Array Date   -- Dates after the last of the month
  , all  :: Array Date   -- The full 35-day range
  }

data BoundaryStatus
  = OutOfBounds
  | InBounds

data CalendarItem
  = CalendarItem SelectableStatus SelectedStatus BoundaryStatus Date

type ChildSlots =
  ( select :: S.Slot Query EmbeddedChildSlots Output Unit
  )

type Component m = H.Component HH.HTML Query Input Output m

type ComponentHTML m = H.ComponentHTML Action ChildSlots m

type ComponentM m a = H.HalogenM State Action ChildSlots Output m a

type ComponentRender m = State -> ComponentHTML m

type CompositeAction = S.Action EmbeddedAction

type CompositeComponent m = H.Component HH.HTML CompositeQuery CompositeInput Output m

type CompositeComponentHTML m = H.ComponentHTML CompositeAction EmbeddedChildSlots m

type CompositeComponentM m a = H.HalogenM CompositeState CompositeAction EmbeddedChildSlots Output m a

type CompositeComponentRender m = CompositeState -> CompositeComponentHTML m

type CompositeInput = S.Input StateRow

type CompositeQuery = S.Query Query EmbeddedChildSlots

type CompositeState = S.State StateRow

data Direction
  = Prev
  | Next

data EmbeddedAction
  = Initialize
  | Key KeyboardEvent
  | OnBlur
  | Receive CompositeInput
  | ToggleMonth Direction
  | ToggleYear  Direction

type EmbeddedChildSlots = () -- NOTE no extension

type Input =
  { disabled :: Boolean
  , interval :: Maybe Interval
  , selection :: Maybe Date
  , targetDate :: Maybe (Year /\ Month)
  }

type Interval =
  { start :: Maybe Date
  , end :: Maybe Date
  }

-- Generates a date range to search through for search term, if it doesn't
-- match on first past it will generate more dates to search through until
-- a specified range limit is reached, then return Nothing
newtype MaxYears = MaxYears Int

data Output
  = SelectionChanged (Maybe Date)
  | VisibilityChanged S.Visibility
  | Searched String

-- NOTE the container and the embedded components share the same query algebra
data Query a
  = GetSelection (Date -> a)
  | SetDisabled Boolean a
  | SetSelection (Maybe Date) a

data SelectableStatus
  = NotSelectable
  | Selectable

data SelectedStatus
  = NotSelected
  | Selected

type Slot = H.Slot Query Output

type Spec m = S.Spec StateRow Query EmbeddedAction EmbeddedChildSlots CompositeInput Output m

type State = Record StateRow

type StateRow =
  ( aligned :: Aligned
  , calendarItems :: Array CalendarItem
  , disabled :: Boolean
  , interval :: Maybe Interval
  , selection :: Maybe Date
  , targetDate :: Year /\ Month
  )

------------
-- Component

component :: forall m. MonadAff m => Component m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , handleQuery = handleQuery
      , receive = Just <<< PassingReceive
      }
  }

---------
-- Values

_select = SProxy :: SProxy "select"

-- Summary helper function that creates a full grid calendar layout
-- from a year and a month.
align :: Year -> Month -> Array (Array Date)
align y m = rowsFromAligned (alignByWeek y m)

-- A special case for when you need to match days of the month to a grid
-- that's bound to weekdays Sun - Sat.
alignByWeek :: Year -> Month -> Aligned
alignByWeek y m = { pre: pre, body: body, post: post, all: pre <> body <> post }
 where
   start = firstDateOfMonth y m
   end = lastDateOfMonth y m
   body = dateRange start end
   pre =
     let pad = padPrev $ weekday start
      in if pad == 0.0 then [] else dateRange (adjustDaysBy pad start) (prevDay start)
   post =
     let pad = padNext $ weekday end
      in if pad == 0.0 then [] else dateRange (nextDay end) (adjustDaysBy pad end)

calendarHeader :: forall m. CompositeComponentHTML m
calendarHeader =
  HH.div
    [ css "flex text-grey-70" ]
    ( headers <#>
      \day ->
        HH.div
          [ css "w-14 h-14 flex items-center justify-center" ]
          [ HH.text day ]
    )
  where
    headers = [ "S", "M", "T", "W", "T", "F", "S" ]

-- Given a string ("Month YYYY"), creates the calendar navigation.
-- Could be much better in rendering
calendarNav :: forall m. Year -> Month -> CompositeComponentHTML m
calendarNav y m =
  Format.contentHeading
    [ css "flex" ]
    [ arrowButton (ToggleMonth Prev) [ Icon.chevronLeft_ ]
    , dateHeader
    , arrowButton (ToggleMonth Next) [ Icon.chevronRight_ ]
    ]
  where
    -- A helper function that will turn a date into a year/month date string
    fmtMonthYear = either (const "-") identity
                   <<< formatDateTime "MMMM YYYY"
                   <<< toDateTime
                   <<< fromDate

    -- We generally will use this value: the current month and year
    monthYear = fmtMonthYear $ canonicalDate y m bottom

    -- We need to embed functionality into these arrow buttons so they trigger
    -- queries in the parent. Let's do that here. To make this work, remember
    -- you're writing in `Select`'s HTML type and you have to wrap your constructors
    -- in Raise.
    arrowButton q =
      Button.buttonClear
        [ HE.onClick $ Just <<< S.Action <<< const q
        , css "text-grey-70 p-3"
        ]

    -- Show the month and year
    dateHeader =
      HH.div
        [ css "flex-1" ]
        [ HH.text monthYear ]

defaultInput :: Input
defaultInput =
  { disabled: false
  , interval: Nothing
  , selection: Nothing
  , targetDate: Nothing
  }

embeddedHandleAction :: forall m. MonadAff m => EmbeddedAction -> CompositeComponentM m Unit
embeddedHandleAction = case _ of
  Initialize -> do
    { interval, selection } <- H.get
    d <- H.liftEffect nowDate
    let
      d' = fromMaybe d selection
      targetDate = (year d') /\ (month d')
      { aligned, calendarItems } = generateCalendarRows interval selection (fst targetDate) (snd targetDate)
    H.modify_
      _ { targetDate = targetDate
        , aligned = aligned
        , calendarItems = calendarItems
        }
    synchronize
  ToggleMonth dir -> toggleMonth dir
  Key ev -> do
    H.modify_ _ { visibility = S.On }
    let preventIt = H.liftEffect $ preventDefault $ KE.toEvent ev
    case KE.code ev of
      "Enter" -> do
        preventIt
        handleSearch
      "Escape" -> do
        preventIt
        H.modify_ _ { visibility = S.Off }
      otherwise -> pure unit
  OnBlur -> do
    state <- H.get
    when (Data.Maybe.isNothing state.selection) do
      handleSearch
    H.modify_ _ { visibility = S.Off }
  Receive input -> embeddedReceive input
  ToggleYear dir -> do
    st <- H.get
    let y = fst st.targetDate
        m = snd st.targetDate
        newDate = case dir of
            Next -> ODT.nextYear (canonicalDate y m bottom)
            Prev -> ODT.prevYear (canonicalDate y m bottom)
    H.modify_ _ { targetDate = Tuple (year newDate) (month newDate) }
    synchronize

embeddedHandleMessage :: forall m. MonadAff m => S.Event -> CompositeComponentM m Unit
embeddedHandleMessage = case _ of
  S.Selected idx -> do
    -- We'll want to select the item here, set its status, and raise
    -- a message about its selection.
    { aligned, calendarItems } <- H.get
    case calendarItems !! idx of
      Nothing -> pure unit
      Just (CalendarItem _ _ _ date) -> do
        when (isInPreviousMonth aligned date) do
          toggleMonth Prev
        when (isInNextMonth aligned date) do
          toggleMonth Next
        H.modify_
          _ { selection = Just date
            , visibility = S.Off
            }
        H.raise $ SelectionChanged $ Just date
        synchronize
  S.Searched text -> do
    H.modify_ _ { search = text }
    -- we don't actually want to match on search, we want to wait
    -- until they hit ENTER and then we'll try to match their search
  S.VisibilityChanged visibility -> do
    H.raise $ VisibilityChanged visibility

embeddedHandleQuery :: forall m a. MonadAff m => Query a -> CompositeComponentM m (Maybe a)
embeddedHandleQuery = case _ of
  GetSelection reply -> do
    { selection }  <- H.get
    pure $ reply <$> selection
  SetDisabled disabled a -> Just a <$ do
    H.modify_ _ { disabled = disabled }
  SetSelection selection a -> Just a <$ do
    setSelectionWithoutRaising selection

embeddedInitialize :: Maybe EmbeddedAction
embeddedInitialize = Just Initialize

-- NOTE configure Select
embeddedInput :: State -> CompositeInput
embeddedInput state =
  { aligned: state.aligned
  , calendarItems: state.calendarItems
  , debounceTime: Nothing
  , disabled: state.disabled
  , getItemCount: Array.length <<< _.calendarItems
  , inputType: S.Text
  , interval: state.interval
  , search: Nothing
  , selection: state.selection
  , targetDate: state.targetDate
  }

embeddedReceive ::
  forall m.
  MonadAff m =>
  CompositeInput ->
  CompositeComponentM m Unit
embeddedReceive input = do
  old <- H.get
  H.modify_ _ { disabled = input.disabled, interval = input.interval }
  case input.interval of
    Nothing -> pure unit
    Just interval -> do
      case old.selection of
        Just selection
          | isWithinInterval interval selection -> pure unit
          | otherwise -> do
            H.modify_ _ { search = ""}
            setSelection Nothing
        Nothing -> pure unit
  synchronize

embeddedRender :: forall m. CompositeComponentRender m
embeddedRender st =
  if st.disabled
    then Input.input [ HP.disabled true, HP.value st.search ]
    else
      HH.div_
      [ renderSearch st.search
      , renderSelect (fst st.targetDate) (snd st.targetDate) st.visibility st.calendarItems
      ]

firstMatch :: Array (Tuple (Fuzzy Date) Int) -> Maybe (Fuzzy Date)
firstMatch = maybe Nothing (Just <<< fst) <<< find match'
  where
    match' (Tuple (Fuzzy { ratio }) _) = ratio == (1 % 1)

generateCalendarItem
  :: Maybe Interval
  -> Maybe Date
  -> BoundaryStatus
  -> Date
  -> CalendarItem
generateCalendarItem mInterval selection bound i = case selection of
  Nothing -> CalendarItem selectableStatus NotSelected bound i
  Just d
    | d == i -> CalendarItem selectableStatus Selected bound i
    | otherwise -> CalendarItem selectableStatus NotSelected bound i
  where
  selectableStatus :: SelectableStatus
  selectableStatus = case mInterval of
    Nothing -> Selectable
    Just interval
      | isWithinInterval interval i -> Selectable
      | otherwise -> NotSelectable

-- Generate a standard set of dates from a year and month.
generateCalendarRows
  :: Maybe Interval
  -> Maybe Date
  -> Year
  -> Month
  -> { calendarItems :: Array CalendarItem, aligned :: Aligned }
generateCalendarRows mInterval selection y m =
  { calendarItems: lastMonth <> thisMonth <> nextMonth
  , aligned
  }
  where
    aligned@{ pre, body, post, all } = alignByWeek y m
    outOfBounds = map (generateCalendarItem mInterval selection OutOfBounds)
    lastMonth   = outOfBounds pre
    nextMonth   = outOfBounds post
    thisMonth = body <#> (generateCalendarItem mInterval selection InBounds)

guessDate :: Date -> MaxYears -> String -> Maybe Date
guessDate start (MaxYears max) text =
  let text' :: String -- replace dashes and slashes with spaces
      text' = either
        (const text)
        (\r -> replace r " " text)
        (regex "-|\\/|," $ parseFlags "g")

      text'' :: String -- consolidate all consecutive whitespaceg
      text'' = either
        (const text')
        (\r -> replace r " " text')
        (regex "\\s+" $ parseFlags "g")

      matcher :: Int -> Date -> Tuple (Fuzzy Date) Int
      matcher i d = Tuple (Data.Fuzzy.match true toObject text'' d) i

      guess :: Array Date -> Int -> Maybe Date
      guess dates = findIn (firstMatch $ sort $ matcher `mapWithIndex` dates)

      findIn :: Maybe (Fuzzy Date) -> Int -> Maybe Date
      findIn (Just (Fuzzy { original })) _ = Just original
      findIn Nothing pass
        | pass > max = Nothing
        | otherwise  = guess (dateRange (yearsForward pass start) (yearsForward (pass + 1) start)) (pass + 1)
   in guess (dateRange start $ nextYear start) 0

-- NOTE re-raise output messages from the embedded component
handleAction :: forall m. Action -> ComponentM m Unit
handleAction = case _ of
  PassingOutput output ->
    H.raise output
  PassingReceive input -> do
    H.modify_ _ { disabled = input.disabled, interval = input.interval }

-- NOTE passing query to the embedded component
handleQuery :: forall m a. Query a -> ComponentM m (Maybe a)
handleQuery = case _ of
  GetSelection reply -> do
    response <- H.query _select unit (S.Query $ H.request GetSelection)
    pure $ reply <$> response
  SetDisabled disabled a -> Just a <$ do
    H.query _select unit (S.Query $ H.tell $ SetDisabled disabled)
  SetSelection selection a -> Just a <$ do
    H.query _select unit (S.Query $ H.tell $ SetSelection selection)

handleSearch :: forall m. MonadAff m => CompositeComponentM m Unit
handleSearch = do
  state <- H.get
  today <- H.liftEffect nowDate
  _ <- case state.search of
    "" -> setSelection Nothing
    _  -> case guessDate today (MaxYears 5) state.search of
      Nothing -> pure unit
      Just d  -> case state.interval of
        Nothing -> setSelection (Just d)
        Just interval
          | isWithinInterval interval d -> setSelection (Just d)
          | otherwise -> pure unit
  H.modify_ _ { visibility = S.Off }
  H.raise $ Searched state.search

initialState :: Input -> State
initialState input =
  let targetDate
        = fromMaybe (ODT.unsafeMkYear 2001 /\ ODT.unsafeMkMonth 1) input.targetDate
      { aligned, calendarItems }= generateCalendarRows input.interval input.selection (fst targetDate) (snd targetDate)
  in
    { aligned
    , calendarItems
    , disabled: input.disabled
    , interval: input.interval
    , selection: input.selection
    , targetDate
    }

isInPreviousMonth :: Aligned -> Date -> Boolean
isInPreviousMonth aligned date = Array.elem date aligned.pre

isInNextMonth :: Aligned -> Date -> Boolean
isInNextMonth aligned date = Array.elem date aligned.post

-- check if a date is within a **closed** interval
isWithinInterval :: Interval -> Date -> Boolean
isWithinInterval interval x =
  Data.Foldable.and
    [ maybe true (_ <= x) interval.start
    , maybe true (x <= _) interval.end
    ]

-- Represents the number of days that will need to be "filled in"
-- when the last day of the month is this weekday. For example, if the
-- last day of the month is Tuesday, then Wednesday through Saturday
-- will need to be padded
padNext :: Weekday -> Number
padNext Sunday    = 6.0
padNext Monday    = 5.0
padNext Tuesday   = 4.0
padNext Wednesday = 3.0
padNext Thursday  = 2.0
padNext Friday    = 1.0
padNext Saturday  = 0.0

-- Represents the number of days that will need to be "filled in"
-- when the first day of the month is this weekday. For example, if the
-- first day of the month is Tuesday, then Sunday and Monday will need
-- to be padded
padPrev :: Weekday -> Number
padPrev Sunday    = 0.0
padPrev Monday    = (-1.0)
padPrev Tuesday   = (-2.0)
padPrev Wednesday = (-3.0)
padPrev Thursday  = (-4.0)
padPrev Friday    = (-5.0)
padPrev Saturday  = (-6.0)

render :: forall m. MonadAff m => ComponentRender m
render st =
  HH.slot _select unit (S.component identity spec) (embeddedInput st) (Just <<< PassingOutput)

renderCalendar :: forall m. Year -> Month -> Array CalendarItem -> CompositeComponentHTML m
renderCalendar y m calendarItems =
  Layout.popover
    ( SS.setContainerProps
      [ HP.classes dropdownClasses ]
    )
    [ calendarNav y m
    , calendarHeader
    , HH.div_ $ renderRows $ rowsFromArray calendarItems
    ]
  where
    dropdownClasses :: Array HH.ClassName
    dropdownClasses = HH.ClassName <$>
      [ "pin-t"
      , "pin-l"
      , "p-6"
      , "bg-white"
      , "text-center"
      , "text-lg"
      ]

renderItem :: forall m. Int -> CalendarItem -> CompositeComponentHTML m
renderItem index item =
  HH.div
  -- Here's the place to use info from the item to render it in different
  -- states.
  -- if highlightedIndex == Just index then 'highlight' else 'dont'
  -- Because there are so many possible states, what about a helper like
  -- getCalendarStyles?
    ( maybeSetItemProps index item
      [ css
        $ trim
        $ "w-14 h-14 rounded-full relative "
          <> "flex items-center justify-center "
          <> "transition-1/4 border border-white "
          <> "before:no-content before:transition-1/4 "
          <> "before:w-full before:h-full "
          <> "before:absolute before:pin-t before:pin-l "
          <> (getCalendarStyles item)
      ]
    )
    -- printDay will format our item correctly
    [ HH.span
      [ css (getLabelStyles item) ]
      [ HH.text $ printDay item ]
    ]
  where
    -- If the calendar item is selectable,
    -- then augment the props with the correct click events.
    -- if not, then just don't provide the props at all.
    -- this is an easy way to "disable" functionality in the calendar.
    maybeSetItemProps i (CalendarItem Selectable _ _ _) props =
      SS.setItemProps i props
    maybeSetItemProps _ _ props = props

    -- Get the correct styles for a calendar item, dependent on its statuses
    getCalendarStyles :: CalendarItem -> String
    getCalendarStyles i
      = trim $ getSelectableStyles i
      <> " " <> getSelectedStyles i
      <> " " <> getBoundaryStyles i
      where
        getSelectableStyles :: CalendarItem -> String
        getSelectableStyles (CalendarItem NotSelectable _ _ _) =
          ""
        getSelectableStyles _ =
          "cursor-pointer hover:border hover:border-blue-88"

        getSelectedStyles :: CalendarItem -> String
        getSelectedStyles (CalendarItem _ Selected _ _) =
          "bg-blue-88 text-white before:scale-1"
        getSelectedStyles _ =
          "before:scale-0"

        getBoundaryStyles :: CalendarItem -> String
        getBoundaryStyles (CalendarItem _ _ OutOfBounds _) =
          "text-grey-90"
        getBoundaryStyles _ = mempty

    getLabelStyles :: CalendarItem -> String
    getLabelStyles = case _ of
      CalendarItem NotSelectable _ InBounds _ ->
        "border-black strike-through"
      CalendarItem NotSelectable _ OutOfBounds _ ->
        "border-grey-90 strike-through"
      _ -> ""

    -- Just a simple helper to format our CalendarItem into a day
    -- we can print out
    printDay :: CalendarItem -> String
    printDay (CalendarItem _ _ _ d) = printDay' d
      where
        printDay' :: Date -> String
        printDay' = (either (const "-") identity)
          <<< formatDateTime "D"
          <<< toDateTime
          <<< fromDate

-- Here we'll render out our dates as rows in the calendar.
renderRows :: forall m. Array (Array CalendarItem) -> Array (CompositeComponentHTML m)
renderRows =
  mapWithIndex (\row subArr -> renderRow (row * 7) subArr)
  where
    renderRow offset items =
      HH.div
        [ css "flex font-light" ]
        ( mapWithIndex
          (\column item -> renderItem (column + offset) item) items
        )

renderSearch :: forall m. String -> CompositeComponentHTML m
renderSearch search =
  Input.input
  $ SS.setInputProps
    [ HE.onBlur \_ -> Just (S.Action OnBlur)
    , HE.onKeyDown $ Just <<< S.Action <<< Key
    , HP.value search
    ]

renderSelect :: forall m. Year -> Month -> S.Visibility -> Array CalendarItem -> CompositeComponentHTML m
renderSelect y m visibility calendarItems =
  HH.div
    [ css "relative" ]
    $ if visibility == S.On
      then [ renderCalendar y m calendarItems ]
      else []

-- Break a set of Sunday-aligned dates into rows, each 7 in length.
rowsFromAligned :: Aligned -> Array (Array Date)
rowsFromAligned { all } = rowsFromArray all

-- Break a set of Sunday-aligned dates into rows, each 7 in length.
rowsFromArray :: ∀ a. Array a -> Array (Array a)
rowsFromArray all = go all []
  where
    go [] acc = reverse acc
    go xs acc = go (drop 7 xs) ([take 7 xs] <> acc)

setSelection :: forall m. MonadAff m => Maybe Date -> CompositeComponentM m Unit
setSelection selection = do
  setSelectionWithoutRaising selection
  H.raise $ SelectionChanged selection

setSelectionWithoutRaising :: forall m. MonadAff m => Maybe Date -> CompositeComponentM m Unit
setSelectionWithoutRaising selection = do
  st <- H.get
  let targetDate = maybe st.targetDate (\d -> (year d) /\ (month d)) selection
  H.modify_ _ { selection = selection, targetDate = targetDate }
  synchronize

spec :: forall m. MonadAff m => Spec m
spec =
  S.defaultSpec
  { render = embeddedRender
  , handleAction = embeddedHandleAction
  , handleQuery = embeddedHandleQuery
  , handleEvent = embeddedHandleMessage
  , initialize = embeddedInitialize
  , receive = Just <<< Receive
  }

synchronize :: forall m. MonadAff m => CompositeComponentM m Unit
synchronize = do
  ({ targetDate: y /\ m, selection, interval }) <- H.get
  let { aligned, calendarItems } = generateCalendarRows interval selection y m
  H.modify_
    _ { aligned = aligned
      , calendarItems = calendarItems
      , highlightedIndex = Nothing
      }
  let update = case selection of
        Nothing -> identity
        Just date -> _ { search = ODT.formatDate date }
  H.modify_ (update <<< _ { calendarItems = calendarItems })

toggleMonth ::
  forall m.
  MonadAff m =>
  Direction ->
  CompositeComponentM m Unit
toggleMonth dir = do
  st <- H.get
  let y = fst st.targetDate
      m = snd st.targetDate
      newDate = case dir of
        Next -> ODT.nextMonth (canonicalDate y m bottom)
        Prev -> ODT.prevMonth (canonicalDate y m bottom)
  H.modify_ _ { targetDate = (year newDate) /\ (month newDate) }
  synchronize

toObject :: Date -> Object String
toObject d =
  fromFoldable
    [ Tuple "mdy1" $ sYearMonth <> " " <> sDay <> " " <> sYear
    , Tuple "mdy2" $ sMonth <> " " <> sDay <> " " <> sYear
    , Tuple "weekday" $ sWeekDay
    , Tuple "wmdy1" $ sWeekDay <> " " <> sYearMonth <> " " <> sDay <> " " <> sYear
    , Tuple "ymd" $ sYear <> " " <> sMonth <> " " <> sDay
    ]
  where
    sYear = show $ fromEnum $ year d
    sMonth = show $ fromEnum $ month d
    sYearMonth = show $ month d
    sDay = show $ fromEnum $ day d
    sWeekDay = show $ weekday d
