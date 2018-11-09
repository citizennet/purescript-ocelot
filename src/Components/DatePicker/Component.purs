module Ocelot.Component.DatePicker where

import Prelude

import Data.Array (mapWithIndex)
import Data.Date (Date, Month, Year, canonicalDate, month, year)
import Data.DateTime.Instant (fromDate, toDateTime)
import Data.Either (either)
import Data.Formatter.DateTime (formatDateTime)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (trim)
import Data.Tuple (Tuple(..), fst, snd)
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
import Ocelot.Component.DatePicker.Utils as Utils
import Ocelot.Data.DateTime as ODT
import Ocelot.HTML.Properties (css)
import Select as Select
import Select.Setters as Setters
import Web.Event.Event (preventDefault)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE

type State =
  { targetDate :: Tuple Year Month
  , selection :: Maybe Date
  , search :: String
  , calendarItems :: Array CalendarItem
  }

type Input =
  { targetDate :: Maybe (Tuple Year Month)
  , selection :: Maybe Date
  }

data Query a
  = HandleSelect (Select.Message Query CalendarItem) a
  | ToggleYear  Direction a
  | ToggleMonth Direction a
  | Initialize a
  | TriggerFocus a
  | Synchronize a
  | GetSelection (Maybe Date -> a)
  | SetSelection (Maybe Date) a
  | Key KeyboardEvent a
  | Search String a

data Message
  = SelectionChanged (Maybe Date)
  | VisibilityChanged Select.Visibility
  | Searched String

data Direction
  = Prev
  | Next

type ParentHTML m
  = H.ParentHTML Query ChildQuery Input m

type ChildSlot = Unit
type ChildQuery = Select.Query Query CalendarItem

----------
-- Calendar Items

data CalendarItem
  = CalendarItem SelectableStatus SelectedStatus BoundaryStatus Date

data SelectableStatus
  = NotSelectable
  | Selectable

data SelectedStatus
  = NotSelected
  | Selected

data BoundaryStatus
  = OutOfBounds
  | InBounds

dropdownClasses :: Array HH.ClassName
dropdownClasses = HH.ClassName <$>
  [ "pin-t"
  , "pin-l"
  , "p-6"
  , "bg-white"
  , "text-center"
  , "text-lg"
  ]

component :: ∀ m
  . MonadAff m
 => H.Component HH.HTML Query Input Message m
component =
  H.lifecycleParentComponent
    { initialState
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just $ H.action Initialize
    , finalizer: Nothing
    }
  where
    initialState :: Input -> State
    initialState { targetDate, selection } =
      let targetDate' = fromMaybe (Tuple (ODT.unsafeMkYear 2001) (ODT.unsafeMkMonth 1)) targetDate
        in
      { targetDate: targetDate'
      , selection
      , search: ""
      , calendarItems: generateCalendarRows selection (fst targetDate') (snd targetDate')
      }

    eval
      :: Query
      ~> H.ParentDSL State Query ChildQuery Unit Message m
    eval = case _ of
      Search text a -> do
        today <- H.liftEffect nowDate
        _ <- case text of
          "" -> eval $ SetSelection Nothing a
          _  -> case Utils.guessDate today (Utils.MaxYears 5) text of
            Nothing -> pure a
            Just d  -> do
              _ <- eval $ SetSelection (Just d) a
              _ <- H.query unit $ Select.setVisibility Select.Off
              pure a
        H.raise $ Searched text
        pure a

      HandleSelect m a -> case m of
        Select.Emit query -> eval query *> pure a

        Select.Selected (CalendarItem _ _ _ date) -> do
          -- We'll want to select the item here, set its status, and raise
          -- a message about its selection.
          H.modify_ _ { selection = Just date }
          _ <- H.query unit $ Select.setVisibility Select.Off
          H.raise $ SelectionChanged $ Just date
          eval $ Synchronize a

        Select.Searched text -> do
          H.modify_ _ { search = text }
          -- we don't actually want to match on search, we want to wait
          -- until they hit ENTER and then we'll try to match their search
          pure a

        Select.VisibilityChanged visibility -> do
          H.raise $ VisibilityChanged visibility
          pure a

      -- We ought to be able to navigate months in the date picker
      ToggleMonth dir a -> a <$ do
        st <- H.get
        let y = fst st.targetDate
            m = snd st.targetDate
            newDate = case dir of
               Next -> ODT.nextMonth (canonicalDate y m bottom)
               Prev -> ODT.prevMonth (canonicalDate y m bottom)
        H.modify_ _ { targetDate = Tuple (year newDate) (month newDate) }
        eval $ Synchronize a

      -- We ought to be able to navigate years in the date picker
      ToggleYear dir a -> a <$ do
        st <- H.get
        let y = fst st.targetDate
            m = snd st.targetDate
            newDate = case dir of
               Next -> ODT.nextYear (canonicalDate y m bottom)
               Prev -> ODT.prevYear (canonicalDate y m bottom)
        H.modify_ _ { targetDate = Tuple (year newDate) (month newDate) }
        eval $ Synchronize a

      -- We can always set the calendar to the current date and time for the user's
      -- convenience. Or just the month.
      Initialize a -> do
        { selection } <- H.get
        d <- H.liftEffect nowDate
        let d' = fromMaybe d selection
        H.modify_ _ { targetDate = Tuple (year d') (month d') }
        eval $ Synchronize a

      TriggerFocus a -> a <$ H.query unit Select.triggerFocus

      Synchronize a -> do
        { targetDate: Tuple y m, selection } <- H.get
        let calendarItems = generateCalendarRows selection y m
        _ <- H.query unit $ Select.replaceItems calendarItems
        let update = case selection of
              Just date -> _ { search = ODT.formatDate date }
              otherwise -> identity
        H.modify_ (update <<< _ { calendarItems = calendarItems })
        pure a

      GetSelection reply -> do
        { selection } <- H.get
        pure $ reply selection

      SetSelection selection a -> do
        st <- H.get
        let targetDate = maybe st.targetDate (\d -> Tuple (year d) (month d)) selection
        H.modify_ _ { selection = selection, targetDate = targetDate }
        eval $ Synchronize a

      Key ev a -> do
        _ <- H.query unit $ Select.setVisibility Select.On
        let preventIt = H.liftEffect $ preventDefault $ KE.toEvent ev
        case KE.code ev of
          "Enter" -> do
            preventIt
            { search } <- H.get
            eval $ Search search a
          "Escape" -> do
            preventIt
            _ <- H.query unit $ Select.setVisibility Select.Off
            pure a
          otherwise -> pure a

    render :: State -> H.ParentHTML Query ChildQuery Unit m
    render st = HH.div_
        [ HH.slot unit Select.component selectInput (HE.input HandleSelect) ]
      where
        selectInput =
          { initialSearch: Nothing
          , debounceTime: Nothing
          , inputType: Select.TextInput
          , items: st.calendarItems
          , render: \s -> HH.div_ [ renderSearch, renderSelect targetYear targetMonth s ]
          }

        targetYear  = fst st.targetDate
        targetMonth = snd st.targetDate

        -- The page element that will hold focus, capture key events, etcetera
        renderSearch =
          Input.input
            ( Setters.setInputProps
              [ HE.onKeyDown $ Just <<< Select.raise <<< H.action <<< Key
              , HP.value st.search
              ]
            )

        renderSelect y m cst =
          HH.div
            [ css "relative" ]
            $ if cst.visibility == Select.On
              then [ renderCalendar y m cst ]
              else [ ]

-- The overall container for the calendar
renderCalendar
  :: Year
  -> Month
  -> Select.State CalendarItem
  -> Select.ComponentHTML Query CalendarItem
renderCalendar y m cst =
  Layout.popover
    ( Setters.setContainerProps
      [ HP.classes dropdownClasses ]
    )
    [ calendarNav
    , calendarHeader
    , HH.div_ $ renderRows $ Utils.rowsFromArray cst.items
    ]
  where
    -- A helper function that will turn a date into a year/month date string
    fmtMonthYear = either (const "-") identity
      <<< formatDateTime "MMMM YYYY"
      <<< toDateTime
      <<< fromDate

    -- We generally will use this value: the current month and year
    monthYear = fmtMonthYear $ canonicalDate y m bottom

    -- Given a string ("Month YYYY"), creates the calendar navigation.
    -- Could be much better in rendering
    calendarNav =
      Format.contentHeading
        [ css "flex" ]
        [ arrowButton (ToggleMonth Prev) [ Icon.chevronLeft_ ]
        , dateHeader
        , arrowButton (ToggleMonth Next) [ Icon.chevronRight_ ]
        ]
      where
        -- We need to embed functionality into these arrow buttons so they trigger
        -- queries in the parent. Let's do that here. To make this work, remember
        -- you're writing in `Select`'s HTML type and you have to wrap your constructors
        -- in Raise.
        arrowButton q =
          Button.buttonClear
            [ HE.onClick $ Select.always $ Select.raise $ H.action q
            , css "text-grey-70 p-3"
            ]

        -- Show the month and year
        dateHeader =
          HH.div
            [ css "flex-1" ]
            [ HH.text monthYear ]

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

    -- Here we'll render out our dates as rows in the calendar.
    renderRows =
      mapWithIndex (\row subArr -> renderRow (row * 7) subArr)
      where
        renderRow offset items =
          HH.div
            [ css "flex font-light" ]
            ( mapWithIndex
              (\column item -> renderItem (column + offset) item) items
            )

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
          Setters.setItemProps i props
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


----------
-- Other helpers for the file

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
