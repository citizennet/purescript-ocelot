module Ocelot.DatePicker where

import Prelude
import Data.Array ((!!), mapWithIndex)
import Data.Array as Array
import Data.Date (Date, Month, Year, canonicalDate, month, year)
import Data.DateTime.Instant (fromDate, toDateTime)
import Data.Either (either)
import Data.Formatter.DateTime (formatDateTime)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (trim)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff.Class (class MonadAff)
import Effect.Now (nowDate)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Ocelot.Block.Button as Button
import Ocelot.Block.Format as Format
import Ocelot.Block.Icon as Icon
import Ocelot.Block.Input as Input
import Ocelot.Block.Layout as Layout
import Ocelot.DatePicker.Utils as Utils
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

type CompositeComponentRender m = CompositeState -> CompositeComponentHTML m

type CompositeComponentM m a = H.HalogenM CompositeState CompositeAction EmbeddedChildSlots Output m a

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
  | ToggleMonth Direction
  | ToggleYear  Direction

type EmbeddedChildSlots = () -- NOTE no extension

type Input =
  { targetDate :: Maybe (Year /\ Month)
  , selection :: Maybe Date
  , disabled :: Boolean
  }

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

type Spec m = S.Spec StateRow Query EmbeddedAction EmbeddedChildSlots CompositeInput Output m

type Slot = H.Slot Query Output

type State = Record StateRow

type StateRow =
  ( targetDate :: Year /\ Month
  , selection :: Maybe Date
  , calendarItems :: Array CalendarItem
  , disabled :: Boolean
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
      }
  }

---------
-- Values

_select = SProxy :: SProxy "select"

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
  { targetDate: Nothing
  , selection: Nothing
  , disabled: false
  }

embeddedHandleAction :: forall m. MonadAff m => EmbeddedAction -> CompositeComponentM m Unit
embeddedHandleAction = case _ of
  Initialize -> do
    { selection } <- H.get
    d <- H.liftEffect nowDate
    let
      d' = fromMaybe d selection
      targetDate = (year d') /\ (month d')
    H.modify_
      _ { targetDate = targetDate
        , calendarItems =
            generateCalendarRows selection (fst targetDate) (snd targetDate)
        }
    synchronize
  ToggleMonth dir -> do
    st <- H.get
    let y = fst st.targetDate
        m = snd st.targetDate
        newDate = case dir of
            Next -> ODT.nextMonth (canonicalDate y m bottom)
            Prev -> ODT.prevMonth (canonicalDate y m bottom)
    H.modify_ _ { targetDate = (year newDate) /\ (month newDate) }
    synchronize
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
    handleSearch
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
    { calendarItems } <- H.get
    case calendarItems !! idx of
      Nothing -> pure unit
      Just (CalendarItem _ _ _ date) -> do
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
    setSelection selection

embeddedInitialize :: Maybe EmbeddedAction
embeddedInitialize = Just Initialize

-- NOTE configure Select
embeddedInput :: State -> CompositeInput
embeddedInput { targetDate, selection, calendarItems, disabled } =
  { inputType: S.Text
  , search: Nothing
  , debounceTime: Nothing
  , getItemCount: Array.length <<< _.calendarItems
  , targetDate
  , selection
  , calendarItems
  , disabled
  }

embeddedRender :: forall m. CompositeComponentRender m
embeddedRender st =
  if st.disabled
    then Input.input [ HP.disabled true, HP.value st.search ]
    else
      HH.div_
      [ renderSearch st.search
      , renderSelect (fst st.targetDate) (snd st.targetDate) st.visibility st.calendarItems
      ]

generateCalendarItem
  :: Maybe Date
  -> BoundaryStatus
  -> Date
  -> CalendarItem
generateCalendarItem Nothing bound i =
  CalendarItem Selectable NotSelected bound i
generateCalendarItem (Just d) bound i
  | d == i = CalendarItem Selectable Selected bound i
  | otherwise = CalendarItem Selectable NotSelected bound i

-- Generate a standard set of dates from a year and month.
generateCalendarRows
  :: Maybe Date
  -> Year
  -> Month
  -> Array CalendarItem
generateCalendarRows selection y m = lastMonth <> thisMonth <> nextMonth
  where
    { pre, body, post, all } = Utils.alignByWeek y m
    outOfBounds = map (generateCalendarItem selection OutOfBounds)
    lastMonth   = outOfBounds pre
    nextMonth   = outOfBounds post
    thisMonth = body <#> (generateCalendarItem selection InBounds)

-- NOTE re-raise output messages from the embedded component
handleAction :: forall m. Action -> ComponentM m Unit
handleAction = case _ of
  PassingOutput output ->
    H.raise output

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
  search <- H.gets _.search
  today <- H.liftEffect nowDate
  _ <- case search of
    "" -> setSelection Nothing
    _  -> case Utils.guessDate today (Utils.MaxYears 5) search of
      Nothing -> pure unit
      Just d  -> do
        setSelection (Just d)
  H.modify_ _ { visibility = S.Off }
  H.raise $ Searched search

initialState :: Input -> State
initialState { targetDate, selection, disabled } =
  let targetDate'
        = fromMaybe (ODT.unsafeMkYear 2001 /\ ODT.unsafeMkMonth 1) targetDate
  in
    { targetDate: targetDate'
    , selection
    , calendarItems: generateCalendarRows selection (fst targetDate') (snd targetDate')
    , disabled
    }

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
    , HH.div_ $ renderRows $ Utils.rowsFromArray calendarItems
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
    [ HH.text $ printDay item ]
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
          mempty
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

setSelection :: forall m. MonadAff m => Maybe Date -> CompositeComponentM m Unit
setSelection selection = do
  st <- H.get
  let targetDate = maybe st.targetDate (\d -> (year d) /\ (month d)) selection
  H.modify_ _ { selection = selection, targetDate = targetDate }
  H.raise $ SelectionChanged selection
  synchronize

spec :: forall m. MonadAff m => Spec m
spec =
  S.defaultSpec
  { render = embeddedRender
  , handleAction = embeddedHandleAction
  , handleQuery = embeddedHandleQuery
  , handleEvent = embeddedHandleMessage
  , initialize = embeddedInitialize
  }

synchronize :: forall m. MonadAff m => CompositeComponentM m Unit
synchronize = do
  ({ targetDate: y /\ m, selection }) <- H.get
  let calendarItems = generateCalendarRows selection y m
  H.modify_
    _ { calendarItems = calendarItems
      , highlightedIndex = Nothing
      }
  let update = case selection of
        Nothing -> identity
        Just date -> _ { search = ODT.formatDate date }
  H.modify_ (update <<< _ { calendarItems = calendarItems })