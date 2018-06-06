module Ocelot.Components.DatePicker where

import Prelude

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff.Now (NOW, now)
import DOM (DOM)
import Data.Array (mapWithIndex)
import Data.Date (Date, Month, Year, canonicalDate, month, year)
import Data.DateTime (date)
import Data.DateTime.Instant (fromDate, toDateTime)
import Data.Either (either)
import Data.Formatter.DateTime (formatDateTime)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Monoid (mempty)
import Data.String (trim)
import Data.Tuple (Tuple(..), fst, snd)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Ocelot.Block.Button as Button
import Ocelot.Block.Format as Format
import Ocelot.Block.Icon as Icon
import Ocelot.Block.Input as Input
import Ocelot.Block.ItemContainer as ItemContainer
import Ocelot.Components.DatePicker.Utils as Utils
import Ocelot.HTML.Properties (css)
import Select as Select
import Select.Utils.Setters as Setters

type Effects eff =
  ( console :: CONSOLE
  , dom :: DOM
  , avar :: AVAR
  , now :: NOW
  | eff
  )

type State =
  { targetDate :: Tuple Year Month
  , selection :: Maybe CalendarItem
  }

data Query a
  = HandleSelect (Select.Message Query CalendarItem) a
  | ToggleYear  Direction a
  | ToggleMonth Direction a
  | SetTime a
  | Synchronize a

data Message
  = SelectionChanged (Maybe CalendarItem)
  | VisibilityChanged Select.Visibility

data Direction
  = Prev
  | Next

type ParentHTML eff m
  = H.ParentHTML Query (ChildQuery eff) Unit m

type ChildQuery eff = Select.Query Query CalendarItem eff


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
  [ "absolute"
  , "shadow"
  , "pin-t"
  , "pin-l"
  , "z-50"
  , "border"
  , "border-grey-90"
  , "rounded"
  , "p-6"
  , "bg-white"
  , "text-center"
  , "text-lg"
  ]

component :: ∀ e m
  . MonadAff ( Effects e ) m
 => H.Component HH.HTML Query Unit Message m
component =
  H.lifecycleParentComponent
    { initialState
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just (H.action SetTime)
    , finalizer: Nothing
    }
  where
    initialState :: Unit -> State
    initialState = const
      { targetDate: Tuple (Utils.unsafeMkYear 2019) (Utils.unsafeMkMonth 2)
      , selection: Nothing
      }

    eval
      :: Query
      ~> H.ParentDSL State Query (ChildQuery (Effects e)) Unit Message m
    eval = case _ of
      HandleSelect m a -> case m of
        Select.Emit query -> eval query *> pure a

        Select.Selected item -> do
          -- We'll want to select the item here, set its status, and raise
          -- a message about its selection.
          let stringify (CalendarItem _ _ _ date) = show date
          H.liftAff $ log ("Selected! Choice was " <> stringify item)
          H.modify $ _ { selection = Just item }
          eval $ Synchronize a

        Select.Searched str -> do
          -- We don't care about searches.
          pure a

        Select.VisibilityChanged vis -> do
          -- Do we care if the visibility changed? We could propagate upward
          -- if the consumer cares.
          pure a

      -- We ought to be able to navigate months in the date picker
      ToggleMonth dir a -> a <$ do
        st <- H.get
        let y = fst st.targetDate
            m = snd st.targetDate
            newDate = case dir of
               Next -> Utils.nextMonth (canonicalDate y m bottom)
               Prev -> Utils.prevMonth (canonicalDate y m bottom)
        H.modify _ { targetDate = Tuple (year newDate) (month newDate) }
        eval $ Synchronize a

      -- We ought to be able to navigate years in the date picker
      ToggleYear dir a -> a <$ do
        st <- H.get
        let y = fst st.targetDate
            m = snd st.targetDate
            newDate = case dir of
               Next -> Utils.nextYear (canonicalDate y m bottom)
               Prev -> Utils.prevYear (canonicalDate y m bottom)
        H.modify _ { targetDate = Tuple (year newDate) (month newDate) }
        eval $ Synchronize a

      -- We can always set the calendar to the current date and time for the user's
      -- convenience. Or just the month.
      SetTime a -> do
        x <- H.liftEff now
        let d = date (toDateTime x)
        H.modify _ { targetDate = Tuple (year d) (month d) }
        eval $ Synchronize a

      Synchronize a -> do
        { targetDate: Tuple y m, selection } <- H.get
        _ <- H.query unit
          $ Select.replaceItems
          $ generateCalendarRows selection y m
        pure a

    render :: State -> H.ParentHTML Query (ChildQuery (Effects e)) Unit m
    render st = HH.div_
        [ HH.slot unit Select.component selectInput (HE.input HandleSelect) ]
      where
        selectInput =
          { initialSearch: Nothing
          , debounceTime: Nothing
          , inputType: Select.Toggle
          , items: generateCalendarRows st.selection targetYear targetMonth
          , render: \s -> HH.div_ [ renderInput, renderSelect targetYear targetMonth s ]
          }

        targetYear  = fst st.targetDate
        targetMonth = snd st.targetDate

        -- The page element that will hold focus, capture key events, etcetera
        renderInput =
          Button.buttonPrimary
            ( Setters.setToggleProps [] )
            [ HH.text "Choose a Date" ]

        renderSelect y m cst =
          HH.div
            [ css "relative" ]
            [ renderCalendar ]
            -- $ if cst.visibility == Select.On
              -- then [ renderCalendar ]
              -- else [ ]
          where
            -- A helper function that will turn a date into a year/month date string
            fmtMonthYear = either (const "-") id
              <<< formatDateTime "MMMM YYYY"
              <<< toDateTime
              <<< fromDate

            -- We generally will use this value: the current month and year
            monthYear = fmtMonthYear $ canonicalDate y m bottom

            -- The overall container for the calendar
            renderCalendar =
              HH.div
                ( Setters.setContainerProps
                  [ HP.classes dropdownClasses ]
                )
                [ calendarNav
                , calendarHeader
                , HH.div_ $ renderRows $ Utils.rowsFromArray cst.items
                ]

            -- Given a string ("Month YYYY"), creates the calendar navigation.
            -- Could be much better in rendering
            calendarNav =
              Format.contentHeading
                [ css "flex" ]
                [ arrowButton (ToggleYear Prev)
                  [ Icon.chevronLeft [ css "-mr-3" ]
                  , Icon.chevronLeft_
                  ]
                , arrowButton (ToggleMonth Prev)
                  [ Icon.chevronLeft_ ]
                , dateHeader
                , arrowButton (ToggleMonth Next)
                  [ Icon.chevronRight_ ]
                , arrowButton (ToggleYear Next)
                  [ Icon.chevronRight [ css "-mr-3" ]
                  , Icon.chevronRight_
                  ]
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
                    getSelectableStyles:: CalendarItem -> String
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
                    printDay' = (either (const "-") id)
                      <<< formatDateTime "D"
                      <<< toDateTime
                      <<< fromDate


----------
-- Other helpers for the file

-- Generate a standard set of dates from a year and month.
generateCalendarRows
  :: Maybe CalendarItem
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
  :: Maybe CalendarItem
  -> BoundaryStatus
  -> Date
  -> CalendarItem
generateCalendarItem Nothing bound i =
  CalendarItem Selectable NotSelected bound i
generateCalendarItem (Just (CalendarItem _ _ _ d)) bound i
  | d == i = CalendarItem Selectable Selected bound i
  | otherwise = CalendarItem Selectable NotSelected bound i
