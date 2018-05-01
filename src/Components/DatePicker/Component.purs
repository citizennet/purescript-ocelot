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
import Data.Tuple (Tuple(..), fst, snd)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Ocelot.Block.Button as Button
import Ocelot.Components.DatePicker.Utils as Utils
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
  { targetDate :: Tuple Year Month }

data Query a
  = HandleSelect (Select.Message Query CalendarItem) a
  | ToggleYear  Direction a
  | ToggleMonth Direction a
  | SetTime a

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


component :: ∀ e m
  . MonadAff ( Effects e ) m
 => H.Component HH.HTML Query Unit Void m
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
      { targetDate: Tuple (Utils.unsafeMkYear 2019) (Utils.unsafeMkMonth 2) }

    eval :: Query ~> H.ParentDSL State Query (ChildQuery (Effects e)) Unit Void m
    eval = case _ of
      HandleSelect m a -> case m of
        Select.Emit query -> do
          eval query
          pure a

        Select.Selected item -> do
          -- We'll want to select the item here, set its status, and raise
          -- a message about its selection.
          let stringify (CalendarItem _ _ _ date) = show date
          H.liftAff $ log ("Selected! Choice was " <> stringify item)
          pure a

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

      -- We ought to be able to navigate years in the date picker
      ToggleYear dir a -> a <$ do
        st <- H.get
        let y = fst st.targetDate
            m = snd st.targetDate
            newDate = case dir of
               Next -> Utils.nextYear (canonicalDate y m bottom)
               Prev -> Utils.prevYear (canonicalDate y m bottom)
        H.modify _ { targetDate = Tuple (year newDate) (month newDate) }

      -- We can always set the calendar to the current date and time for the user's
      -- convenience. Or just the month.
      SetTime a -> do
         x <- H.liftEff now
         let d = date (toDateTime x)
         H.modify _ { targetDate = Tuple (year d) (month d) }
         pure a

    render :: State -> H.ParentHTML Query (ChildQuery (Effects e)) Unit m
    render st = HH.div_
        [ HH.slot unit Select.component selectInput (HE.input HandleSelect) ]
      where
        selectInput =
          { initialSearch: Nothing
          , debounceTime: Nothing
          , inputType: Select.Toggle
          , items: generateCalendarRows targetYear targetMonth
          , render: \s -> HH.div_ [ renderToggle, renderSelect targetYear targetMonth s ]
          }

        targetYear  = fst st.targetDate
        targetMonth = snd st.targetDate

        -- The page element that will hold focus, capture key events, etcetera
        renderToggle =
          Button.buttonPrimary
            ( Setters.setToggleProps [] )
            [ HH.text "Choose Date" ]

        renderSelect y m cst =
          HH.div_
            $ if cst.visibility == Select.Off
              then [ ]
              else [ renderCalendar ]
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
                ( Setters.setContainerProps [] )
                [ calendarNav
                , calendarHeader
                , HH.div_ $ renderRows $ Utils.rowsFromArray cst.items
                ]

            -- Given a string ("Month YYYY"), creates the calendar navigation.
            -- Could be much better in rendering
            calendarNav =
              HH.div
              [ ]
              [ arrowButton (ToggleYear Prev) "<<"
              , arrowButton (ToggleMonth Prev) "<"
              , dateHeader
              , arrowButton (ToggleMonth Next) ">"
              , arrowButton (ToggleYear Next) ">>"
              ]
              where
                -- We need to embed functionality into these arrow buttons so they trigger
                -- queries in the parent. Let's do that here. To make this work, remember
                -- you're writing in `Select`'s HTML type and you have to wrap your constructors
                -- in Raise.
                arrowButton q t =
                  HH.button
                    [ HE.onClick $ HE.input_ $ Select.Raise $ H.action q ]
                    [ HH.text t ]

                -- Show the month and year
                dateHeader =
                  HH.div
                  [ ]
                  [ HH.text monthYear ]

            calendarHeader =
              HH.div
              [ ]
              ( headers <#> \day -> HH.div [ ] [ HH.text day ] )
              where
                headers = [ "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat" ]

            -- Here we'll render out our dates as rows in the calendar.
            renderRows = mapWithIndex (\row subArr -> renderRow (row * 7) subArr)
              where
                renderRow offset items =
                  HH.div
                    [ HP.class_ $ HH.ClassName "flex" ]
                    ( mapWithIndex (\column item -> renderItem (column + offset) item) items )

            renderItem index item =
              HH.div
              -- Here's the place to use info from the item to render it in different
              -- states.
              -- if highlightedIndex == Just index then 'highlight' else 'dont'
              -- Because there are so many possible states, what about a helper like
              -- getCalendarStyles?
                ( maybeSetItemProps index item
                  [ HP.attr (H.AttrName "style") (getCalendarStyles item) ]
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
                  =  getSelectableStyles i
                  <> " " <> getSelectedStyles i
                  <> " " <> getBoundaryStyles i
                  where
                    getSelectableStyles:: CalendarItem -> String
                    getSelectableStyles (CalendarItem NotSelectable _ _ _) =
                      mempty -- replace with something
                    getSelectableStyles _ = mempty

                    getSelectedStyles :: CalendarItem -> String
                    getSelectedStyles (CalendarItem _ Selected _ _) =
                      mempty -- replace with something
                    getSelectedStyles _ = mempty

                    getBoundaryStyles :: CalendarItem -> String
                    getBoundaryStyles (CalendarItem _ _ OutOfBounds _) =
                      mempty -- replace with something
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
generateCalendarRows :: Year -> Month -> Array CalendarItem
generateCalendarRows y m = lastMonth <> thisMonth <> nextMonth
  where
    { pre, body, post, all } = Utils.alignByWeek y m
    outOfBounds = map (\i -> CalendarItem Selectable NotSelected OutOfBounds i)
    lastMonth   = outOfBounds pre
    nextMonth   = outOfBounds post
    thisMonth = body <#> (\i -> CalendarItem Selectable NotSelected InBounds i)
