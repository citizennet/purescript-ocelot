module Ocelot.Component.TimePicker where

import Prelude

import Data.FunctorWithIndex (mapWithIndex)
import Data.Maybe (Maybe(..))
import Data.String (trim)
import Data.Time (Time)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Ocelot.Block.Input as Input
import Ocelot.Block.Layout as Layout
import Ocelot.Component.TimePicker.Utils as Utils
import Ocelot.Data.DateTime as ODT
import Ocelot.HTML.Properties (css)
import Select as Select
import Select.Setters as Setters
import Web.Event.Event (preventDefault)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE

type State =
  { selection :: Maybe Time
  , search :: String
  , timeUnits :: Array TimeUnit
  }

type Input =
  { selection :: Maybe Time
  }

data Query a
  = HandleSelect (Select.Message Query TimeUnit) a
  | TriggerFocus a
  | Synchronize a
  | GetSelection (Maybe Time -> a)
  | SetSelection (Maybe Time) a
  | Key KeyboardEvent a
  | Search String a

data Message
  = SelectionChanged (Maybe Time)
  | VisibilityChanged Select.Visibility
  | Searched String

type ParentHTML m
  = H.ParentHTML Query ChildQuery Input m

type ChildSlot = Unit
type ChildQuery = Select.Query Query TimeUnit

----------
-- Time Units

data TimeUnit
  = TimeUnit SelectableStatus SelectedStatus Time

data SelectableStatus
  = NotSelectable
  | Selectable

data SelectedStatus
  = NotSelected
  | Selected

dropdownClasses :: Array HH.ClassName
dropdownClasses = HH.ClassName <$>
  [ "max-h-80"
  , "w-full"
  , "overflow-y-scroll"
  , "pin-t"
  , "pin-l"
  , "bg-white"
  , "text-center"
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
    , initializer: Nothing
    , finalizer: Nothing
    }
  where
    initialState :: Input -> State
    initialState { selection } =
      { selection
      , search: ""
      , timeUnits: generateTimes selection
      }

    eval
      :: Query
      ~> H.ParentDSL State Query ChildQuery Unit Message m
    eval = case _ of
      Search text a -> do
        _ <- case text of
          "" -> eval $ SetSelection Nothing a
          _  -> case Utils.guessTime text of
            Nothing -> pure a
            Just t  -> do
              _ <- eval $ SetSelection (Just t) a
              _ <- H.query unit $ Select.setVisibility Select.Off
              pure a
        H.raise $ Searched text
        pure a

      HandleSelect m a -> case m of
        Select.Emit query -> eval query *> pure a

        Select.Selected (TimeUnit _ _ time) -> do
          -- We'll want to select the item here, set its status, and raise
          -- a message about its selection.
          H.modify_ _ { selection = Just time }
          _ <- H.query unit $ Select.setVisibility Select.Off
          H.raise $ SelectionChanged $ Just time
          eval $ Synchronize a

        Select.Searched text -> do
          H.modify_ _ { search = text }
          -- we don't actually want to match on search, we want to wait
          -- until they hit ENTER and then we'll try to match their search
          pure a

        Select.VisibilityChanged visibility -> do
          H.raise $ VisibilityChanged visibility
          pure a

      TriggerFocus a -> a <$ H.query unit Select.triggerFocus

      Synchronize a -> do
        { selection } <- H.get
        let timeUnits = generateTimes selection
        _ <- H.query unit $ Select.replaceItems timeUnits
        let update = case selection of
              Just time -> _ { search = ODT.formatTime time }
              otherwise -> identity
        H.modify_ (update <<< _ { timeUnits = timeUnits })
        pure a

      GetSelection reply -> do
        { selection } <- H.get
        pure $ reply selection

      SetSelection selection a -> do
        H.modify_ _ { selection = selection }
        eval $ Synchronize a

      Key ev a -> do
        _ <- H.query unit $ Select.setVisibility Select.On
        let preventIt = H.liftEffect $ preventDefault $ KE.toEvent ev
        case KE.code ev of
          "Enter"   -> do
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
          , items: st.timeUnits
          , render: \s -> HH.div_ [ renderSearch, renderSelect s ]
          }

        -- The page element that will hold focus, capture key events, etcetera
        renderSearch =
          Input.input
            ( Setters.setInputProps
              [ HE.onKeyDown $ Just <<< Select.raise <<< H.action <<< Key
              , HP.value st.search
              ]
            )

        renderSelect tst =
          HH.div
            [ css "relative" ]
            $ if tst.visibility == Select.On
              then [ renderTimes tst ]
              else [ ]

-- The overall container for the time dropdown
renderTimes :: Select.State TimeUnit -> Select.ComponentHTML Query TimeUnit
renderTimes tst =
  Layout.popover
    ( Setters.setContainerProps
      [ HP.classes dropdownClasses ]
    )
    ( mapWithIndex renderItem tst.items )

renderItem :: Int -> TimeUnit -> Select.ComponentHTML Query TimeUnit
renderItem index item =
  HH.div
  -- Here's the place to use info from the item to render it in different
  -- states.
  -- if highlightedIndex == Just index then 'highlight' else 'dont'
    ( maybeSetItemProps index item
      [ css
        $ trim
        $ "relative p-3 transition-1/4 "
          <> (getTimeStyles item)
      ]
    )
    -- printDay will format our item correctly
    [ HH.text $ printTime item ]
  where
    -- If the timeunit is selectable,
    -- then augment the props with the correct click events.
    -- if not, then just don't provide the props at all.
    -- this is an easy way to "disable" functionality in the calendar.
    maybeSetItemProps i (TimeUnit Selectable _ _) props =
      Setters.setItemProps i props
    maybeSetItemProps _ _ props = props

    -- Get the correct styles for a time unit, dependent on its statuses
    getTimeStyles :: TimeUnit -> String
    getTimeStyles i
      = trim $ getSelectableStyles i
      <> " " <> getSelectedStyles i
      where
        getSelectableStyles :: TimeUnit -> String
        getSelectableStyles (TimeUnit NotSelectable _ _) =
          mempty
        getSelectableStyles _ =
          "cursor-pointer hover:bg-grey-97"

        getSelectedStyles :: TimeUnit -> String
        getSelectedStyles (TimeUnit _ Selected _) =
          "text-blue-88"
        getSelectedStyles _ =
          mempty

    -- Just a simple helper to format our TimeUnit into a day
    -- we can print out
    printTime :: TimeUnit -> String
    printTime (TimeUnit _ _ t) = ODT.formatTime t


----------
-- Other helpers for the file

-- Generate a standard set of time intervals.
generateTimes
  :: Maybe Time
  -> Array TimeUnit
generateTimes selection =
  ODT.defaultTimeRange <#> (generateTimeUnit selection)

generateTimeUnit
  :: Maybe Time
  -> Time
  -> TimeUnit
generateTimeUnit Nothing i =
  TimeUnit Selectable NotSelected i
generateTimeUnit (Just t) i
  | t == i = TimeUnit Selectable Selected i
  | otherwise = TimeUnit Selectable NotSelected i
