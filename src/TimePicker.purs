module Ocelot.TimePicker
  ( Action 
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
  , CompositeState
  , CompositeQuery
  , EmbeddedAction(..)
  , EmbeddedChildSlots
  , Input
  , Output(..)
  , Query(..)
  , Slot
  , Spec
  , State
  , StateRow
  , TimeUnit
  , component
  ) where

import Prelude
import Control.Alt ((<|>))
import Data.Array ((!!), length)
import Data.Array as Array
import Data.Array.NonEmpty (catMaybes, head)
import Data.DateTime (time)
import Data.Either (either, hush)
import Data.Formatter.DateTime (unformatDateTime)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Data.String (joinWith, toLower, trim)
import Data.String.Regex (match, parseFlags, regex)
import Data.Time (Time)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Ocelot.Block.Input as Input
import Ocelot.Block.Layout as Layout
import Ocelot.Data.DateTime as ODT
import Ocelot.HTML.Properties (css)
import Select as S
import Select.Setters as Setters
import Type.Proxy (Proxy(..))
import Web.Event.Event (preventDefault)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE

--------
-- Types

data Action
  = PassingOutput Output

type ChildSlots =
  ( select :: S.Slot Query EmbeddedChildSlots Output Unit
  )

type Component m = H.Component Query Input Output m

type ComponentHTML m = H.ComponentHTML Action ChildSlots m

type ComponentM m a = H.HalogenM State Action ChildSlots Output m a

type ComponentRender m = State -> ComponentHTML m

type CompositeAction = S.Action EmbeddedAction

type CompositeComponent m = H.Component CompositeQuery CompositeInput Output m

type CompositeComponentHTML m = H.ComponentHTML CompositeAction EmbeddedChildSlots m

type CompositeComponentM m a = H.HalogenM CompositeState CompositeAction EmbeddedChildSlots Output m a

type CompositeComponentRender m = CompositeState -> CompositeComponentHTML m

type CompositeInput = S.Input StateRow

type CompositeState = S.State StateRow

type CompositeQuery = S.Query Query EmbeddedChildSlots

data EmbeddedAction
  = Initialize
  | Key KeyboardEvent
  | OnBlur

type EmbeddedChildSlots = () -- No extension

type Input =
  { selection :: Maybe Time
  , disabled :: Boolean
  }

data Meridiem
  = AM
  | PM

data Output
  = SelectionChanged (Maybe Time)
  | VisibilityChanged S.Visibility
  | Searched String

data Query a
  = GetSelection (Time -> a)
  | SetDisabled Boolean a
  | SetSelection (Maybe Time) a

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
  ( selection :: Maybe Time
  , timeUnits :: Array TimeUnit
  , disabled :: Boolean
  )

data TimeUnit
  = TimeUnit SelectableStatus SelectedStatus Time

-------------
-- Components

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

_select = Proxy :: Proxy "select"

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

embeddedHandleAction :: forall m. MonadAff m => EmbeddedAction -> CompositeComponentM m Unit
embeddedHandleAction = case _ of
  Initialize -> do
    synchronize
  Key ev -> do
    H.modify_ _ { visibility = S.Off }
    let preventIt = H.liftEffect $ preventDefault $ KE.toEvent ev
    case KE.code ev of
      "Enter"   -> do
        preventIt
        handleSearch
      "Escape" -> do
        preventIt
        H.modify_ _ { visibility = S.Off }
      otherwise -> pure unit
  OnBlur -> do
    { selection } <- H.get
    when (isNothing selection) handleSearch

embeddedHandleMessage
  :: forall m
   . S.Event
  -> CompositeComponentM m Unit
embeddedHandleMessage = case _ of
  S.Selected idx -> do
    -- We'll want to select the item here, set its status, and raise
    -- a message about its selection.
    { timeUnits } <- H.get
    case timeUnits !! idx of
      Nothing -> pure unit
      Just (TimeUnit _ _ time) -> do
        H.modify_ _ { selection = Just time, visibility = S.Off }
        H.raise $ SelectionChanged $ Just time
        synchronize
  S.Searched text -> do
    H.modify_ _ { search = text, selection = Nothing }
    -- we don't actually want to match on search, we want to wait
    -- until they hit ENTER and then we'll try to match their search
  S.VisibilityChanged visibility -> do
    H.raise $ VisibilityChanged visibility

embeddedHandleQuery :: forall m a. Query a -> CompositeComponentM m (Maybe a)
embeddedHandleQuery = case _ of
  GetSelection reply -> do
    { selection } <- H.get
    pure $ reply <$> selection
  SetDisabled disabled a -> Just a <$ do
    H.modify_ _ { disabled = disabled }
  SetSelection selection a -> Just a <$ do
    setSelectionWithoutRaising selection

embeddedInput :: State -> CompositeInput
embeddedInput { selection, timeUnits, disabled } =
  { inputType: S.Text
  , search: Nothing
  , debounceTime: Nothing
  , getItemCount: Array.length <<< _.timeUnits
  , selection
  , timeUnits
  , disabled
  }

embeddedRender :: forall m. CompositeComponentRender m
embeddedRender s =
  if s.disabled
    then Input.input [ HP.disabled true, HP.value s.search ]
    else
      HH.div_
        [ renderSearch s.search
        , renderSelect s.visibility s.timeUnits
        ]

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

guessTime :: String -> Maybe Time
guessTime ""   = Nothing
guessTime text =
  let meridiem :: Maybe Meridiem
      meridiem = do
        let
          regexFlags = parseFlags "i"
        regexA <- hush (regex "a" $ regexFlags)
        regexP <- hush (regex "p" $ regexFlags)
        matched <- match regexA text <|> match regexP text
        meridiemString <- head matched
        case toLower meridiemString of
          "a" -> Just AM
          "p" -> Just PM
          _ -> Nothing

      digits :: Array String
      digits = either
        (const [])
        (\r -> fromMaybe [] (catMaybes <$> match r text))
        (regex "\\d" $ parseFlags "g")

      digits' :: String
      digits' = joinWith "" digits

      hourMin :: Maybe String
      hourMin = case length digits of
        1 -> pure $ "0" <> digits' <> "00"
        2 -> pure $ digits' <> "00"
        3 -> pure $ "0" <> digits'
        4 -> pure $ digits'
        _ -> Nothing

      format :: String
      format = (maybe "HHmm" (const "hhmm a") meridiem)

      suffix :: String
      suffix = maybe "" (\m -> " " <> meridiemToString m) meridiem

      guess :: Maybe String
      guess = (_ <> suffix) <$> hourMin
   in time <$> (join $ hush <<< unformatDateTime format <$> guess)

handleAction :: forall m. Action -> ComponentM m Unit
handleAction = case _ of
  PassingOutput output ->
    H.raise output

handleQuery :: forall m a. Query a -> ComponentM m (Maybe a)
handleQuery = case _ of
  GetSelection reply -> do
    response <- H.request _select unit (S.Query <<< GetSelection)
    pure $ reply <$> response
  SetDisabled disabled a -> Just a <$ do
    void $ H.tell _select unit (S.Query <<<  SetDisabled disabled)
  SetSelection selection a -> Just a <$ do
    H.tell _select unit (S.Query <<< SetSelection selection)

handleSearch :: forall m. MonadAff m => CompositeComponentM m Unit
handleSearch = do
  search <- H.gets _.search
  case search of
    "" -> setSelection Nothing
    _  -> case guessTime search of
      Nothing -> pure unit
      Just t  -> do
        setSelection (Just t)
        H.modify_ _ { visibility = S.Off }
  H.raise $ Searched search

initialState :: Input -> State
initialState { selection, disabled } =
  { selection
  , timeUnits: generateTimes selection
  , disabled
  }

meridiemToString :: Meridiem -> String
meridiemToString = case _ of
  AM -> "AM"
  PM -> "PM"

render :: forall m. MonadAff m => ComponentRender m
render st =
    HH.slot _select unit (S.component identity spec) (embeddedInput st) (Just <<< PassingOutput)

renderItem :: forall m. Int -> TimeUnit -> CompositeComponentHTML m
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

-- The page element that will hold focus, capture key events, etcetera
renderSearch :: forall m. String -> CompositeComponentHTML m
renderSearch search =
  Input.input
    ( Setters.setInputProps
      [ HE.onBlur \_ -> Just (S.Action OnBlur)
      , HE.onKeyDown $ Just <<< S.Action <<< Key
      , HP.value search
      ]
    )

renderSelect :: forall m. S.Visibility -> Array TimeUnit -> CompositeComponentHTML m
renderSelect visibility timeUnits =
  HH.div
    [ css "relative" ]
    $ if visibility == S.On
      then [ renderTimes ]
      else [ ]
  where
  -- The overall container for the time dropdown
  renderTimes =
    Layout.popover
      ( Setters.setContainerProps
        [ HP.classes dropdownClasses ]
      )
      ( mapWithIndex renderItem timeUnits )

setSelection :: forall m. Maybe Time -> CompositeComponentM m Unit
setSelection selection = do
  setSelectionWithoutRaising selection
  H.raise $ SelectionChanged selection

setSelectionWithoutRaising :: forall m. Maybe Time -> CompositeComponentM m Unit
setSelectionWithoutRaising selection = do
  H.modify_ _ { selection = selection }
  synchronize

spec :: forall m. MonadAff m => Spec m
spec = S.defaultSpec
  { render = embeddedRender
  , handleAction = embeddedHandleAction
  , handleQuery = embeddedHandleQuery
  , handleEvent = embeddedHandleMessage
  , initialize = Just Initialize
  }

synchronize :: forall m. CompositeComponentM m Unit
synchronize = do
  { selection } <- H.get
  H.modify_ _ { timeUnits = generateTimes selection }
  case selection of
    Nothing -> pure unit
    Just time -> H.modify_ _ { search = ODT.formatTime time }
