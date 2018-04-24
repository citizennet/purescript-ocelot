module Ocelot.Components.DatePicker where

import Prelude
import Ocelot.Utils.Date (alignByWeek, nextMonth, nextYear, prevMonth, prevYear, rowsFromArray, unsafeMkYear, unsafeMkMonth)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Aff.Console (log, CONSOLE)
import DOM (DOM)
import Data.Array (mapWithIndex)
import Data.Date (Date, Month, Year, canonicalDate, month, year)
import Data.DateTime (date)
import Data.DateTime.Instant (fromDate, toDateTime)
import Data.Either (either)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (mempty)
import Data.Tuple (Tuple(..), fst, snd)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type Effects eff = ( console :: CONSOLE, dom :: DOM | eff )

type State =
  { targetDate :: Tuple Year Month }

data Query a
  = ToggleYear  Direction a
  | ToggleMonth Direction a
  | SetTime a

data Direction = Prev | Next

type ParentHTML m = H.ParentHTML Query ChildQuery Unit m
type ChildQuery = Query -- C.ContainerQuery Query CalendarItem

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


component :: ∀ e m. MonadAff ( Effects e ) m => H.Component HH.HTML Query Unit Void m
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
      { targetDate: Tuple (unsafeMkYear 2019) (unsafeMkMonth 2) }

    eval :: Query ~> H.ParentDSL State Query ChildQuery Unit Void m
    eval = case _ of
      --  HandleSelect m a -> pure a

      ToggleMonth dir a -> a <$ do
        st <- H.get

        let y = fst st.targetDate
            m = snd st.targetDate

        let newDate = case dir of
               Next -> nextMonth (canonicalDate y m bottom)
               Prev -> prevMonth (canonicalDate y m bottom)

        H.modify _ { targetDate = Tuple (year newDate) (month newDate) }

      ToggleYear dir a -> a <$ do
        st <- H.get

        let y = fst st.targetDate
            m = snd st.targetDate

        let newDate = case dir of
               Next -> nextYear (canonicalDate y m bottom)
               Prev -> prevYear (canonicalDate y m bottom)

        H.modify _ { targetDate = Tuple (year newDate) (month newDate) }

      SetTime a -> do
         --  x <- H.liftEff now
         --  let d = date (toDateTime x)
         --  H.modify _ { targetDate = Tuple (year d) (month d) }
         pure a


    render :: State -> H.ParentHTML Query ChildQuery Unit m
    render st = HH.div_ []

      where
        targetYear  = fst st.targetDate
        targetMonth = snd st.targetDate

        renderToggle :: H.ParentHTML Query ChildQuery Unit m
        renderToggle =
          HH.span_
          [ HH.text "Toggle" ]

        -- The user is using the Container primitive, so they have to fill out a Container render function
        --  renderContainer :: Year -> Month -> (C.ContainerState CalendarItem) -> H.HTML Void ChildQuery
        --  renderContainer y m cst =
        --    HH.div_
        --      $ if not cst.open
        --        then [ ]
        --        else [ renderCalendar ]
        --    where
        --      fmtMonthYear = (either (const "-") id) <<< formatDateTime "MMMM YYYY" <<< toDateTime <<< fromDate
        --      monthYear = fmtMonthYear (canonicalDate y m bottom)
        --
        --      renderCalendar :: H.HTML Void ChildQuery
        --      renderCalendar =
        --        HH.div
        --          ( C.getContainerProps
        --            [ HP.class_ $ HH.ClassName "tc"
        --            , HC.style  $ CSS.width (CSS.rem 28.0) ]
        --          )
        --          [ calendarNav
        --          , calendarHeader
        --          , HH.div_ $ renderRows $ rowsFromArray cst.items
        --          ]
        --
        --      -- Given a string ("Month YYYY"), creates the calendar navigation
        --      calendarNav :: H.HTML Void ChildQuery
        --      calendarNav =
        --        HH.div
        --        [ HP.class_ $ HH.ClassName "flex pv3" ]
        --        [ arrowButton (ToggleYear Prev) "<<" (Just "ml2")
        --        , arrowButton (ToggleMonth Prev) "<" Nothing
        --        , dateHeader
        --        , arrowButton (ToggleMonth Next) ">" Nothing
        --        , arrowButton (ToggleYear Next) ">>" (Just "mr2")
        --        ]
        --        where
        --          arrowButton q t css =
        --            HH.button
        --            ( C.getChildProps
        --              [ HP.class_ $ HH.ClassName $ "w-10" <> fromMaybe "" (((<>) " ") <$> css)
        --              , HE.onClick $ HE.input_ $ C.Raise $ H.action q ]
        --            )
        --            [ HH.text t ]
        --
        --          -- Show the month and year
        --          dateHeader =
        --            HH.div
        --            [ HP.class_ $ HH.ClassName "w-60 b" ]
        --            [ HH.text monthYear ]
        --
        --      calendarHeader =
        --        HH.div
        --        [ HP.class_ $ HH.ClassName "flex pv3" ]
        --        ( headers <#> (\day -> HH.div [ HP.class_ $ HH.ClassName "w3" ] [ HH.text day ]) )
        --        where
        --          headers = [ "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat" ]
        --
        --      renderRows :: Array (Array CalendarItem) -> Array (H.HTML Void ChildQuery)
        --      renderRows = mapWithIndex (\row subArr -> renderRow (row * 7) subArr)
        --        where
        --          renderRow :: Int -> Array CalendarItem -> H.HTML Void ChildQuery
        --          renderRow offset items =
        --            HH.div
        --              [ HP.class_ $ HH.ClassName "flex" ]
        --              ( mapWithIndex (\column item -> renderItem (column + offset) item) items )
        --
        --      renderItem :: Int -> CalendarItem -> H.HTML Void ChildQuery
        --      renderItem index item =
        --        HH.div
        --          -- Use raw style attribute for convenience.
        --          ( attachItemProps index item
        --          [ HP.class_ $ HH.ClassName $ "w3 pa3" <> (if cst.highlightedIndex == Just index then " bg-washed-green" else "")
        --          , HP.attr (H.AttrName "style") (getCalendarStyles item) ]
        --          )
        --          [ HH.text $ printDay item ]
        --        where
        --          -- If the calendar item is selectable, augment the props with the correct click events.
        --          attachItemProps i (CalendarItem Selectable _ _ _) props = C.getItemProps i props
        --          attachItemProps _ _ props = props
        --
        --          -- Get the correct styles for a calendar item dependent on its statuses
        --          getCalendarStyles :: CalendarItem -> String
        --          getCalendarStyles i
        --            =  getSelectableStyles i
        --            <> " " <> getSelectedStyles i
        --            <> " " <> getBoundaryStyles i
        --            where
        --              getSelectableStyles:: CalendarItem -> String
        --              getSelectableStyles (CalendarItem NotSelectable _ _ _)
        --                = "color: rgba(0,0,0,0.6); background-image: linear-gradient(to bottom, rgba(125,125,125,0.75) 0%, rgba(125,125,125,0.75), 100%;"
        --              getSelectableStyles _ = mempty
        --
        --              getSelectedStyles :: CalendarItem -> String
        --              getSelectedStyles (CalendarItem _ Selected _ _) = "color: white; background-color: green;"
        --              getSelectedStyles _ = mempty
        --
        --              getBoundaryStyles :: CalendarItem -> String
        --              getBoundaryStyles (CalendarItem _ _ OutOfBounds _) = "opacity: 0.5;"
        --              getBoundaryStyles _ = mempty
        --
        --          printDay :: CalendarItem -> String
        --          printDay (CalendarItem _ _ _ d) = printDay' d
        --            where
        --              printDay' :: Date -> String
        --              printDay' = (either (const "-") id)
        --                <<< formatDateTime "D"
        --                <<< toDateTime
        --                <<< fromDate


{-
Helpers
-}

-- Generate a standard set of dates from a year and month.
generateCalendarRows :: Year -> Month -> Array CalendarItem
generateCalendarRows y m = lastMonth <> thisMonth <> nextMonth
  where
    { pre, body, post, all } = alignByWeek y m
    outOfBounds = map (\i -> CalendarItem Selectable NotSelected OutOfBounds i)
    lastMonth   = outOfBounds pre
    nextMonth   = outOfBounds post
    thisMonth = body <#> (\i -> CalendarItem Selectable NotSelected InBounds i)
