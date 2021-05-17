module Ocelot.Component.SearchBar where

import Prelude

import Control.Monad.State (class MonadState)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (null)
import Data.String as Data.String
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (Fiber, delay, forkAff, killFiber)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff)
import Effect.Exception (error)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Ocelot.Block.Icon as Icon
import Ocelot.HTML.Properties (css)
import Web.Event.Event (stopPropagation)
import Web.UIEvent.MouseEvent as ME

type State =
  { query :: String
  , debouncer :: Maybe Debouncer
  , debounceTime :: Milliseconds
  , open :: Boolean
  , keepOpen :: Boolean
  }

type Debouncer =
  { var :: AVar String
  , fiber :: Fiber Unit
  }


data Action
  = Clear ME.MouseEvent
  | Search String
  | Open
  | Blur

data Query a
  = SetText String a

type Slot = H.Slot Query Message

type Input = { debounceTime :: Maybe Milliseconds }
type Input' =
  { debounceTime :: Maybe Milliseconds
  , keepOpen :: Boolean
  }

data Message
 = Searched String

-- | The standard search bar
component :: ∀ m. MonadAff m => H.Component Query Input Message m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval (H.defaultEval { handleQuery = handleQuery, handleAction = handleAction })
    }

  where
    initialState :: Input -> State
    initialState { debounceTime } =
      { query: ""
      , debouncer: Nothing
      , debounceTime: fromMaybe (Milliseconds 0.0) debounceTime
      , open: false
      , keepOpen: false
      }

-- | A search bar which allows the user to specify if it should
-- | stay open when unfocused
component' :: ∀ m. MonadAff m => H.Component Query Input' Message m
component' =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval (H.defaultEval { handleQuery = handleQuery, handleAction = handleAction })
    }

  where
    initialState :: Input' -> State
    initialState { debounceTime, keepOpen } =
      { query: ""
      , debouncer: Nothing
      , debounceTime: fromMaybe (Milliseconds 0.0) debounceTime
      , open: keepOpen
      , keepOpen
      }

handleAction :: forall m.
  MonadAff m =>
  Action ->
  H.HalogenM State Action () Message m Unit
handleAction = case _ of
  Open -> do
    H.modify_ _ { open = true }

  Blur -> do
    { query } <- H.get
    closeIfNullQuery query

  Clear ev -> do
    H.liftEffect $ stopPropagation $ ME.toEvent ev
    H.modify_ \st -> st { query = "", open = st.keepOpen }
    H.raise $ Searched ""

  Search str -> do
    H.modify_ _ { query = str }
    openIfHasQuery str
    st <- H.get

    case st.debouncer of
      Nothing -> unit <$ do
        var <- H.liftAff AVar.empty
        fiber <- H.liftAff $ forkAff do
          delay st.debounceTime
          AVar.put str var

        _ <- H.fork do
          val <- H.liftAff $ AVar.take var
          H.modify_ _ { debouncer = Nothing }
          H.raise $ Searched val

        H.modify_ _ { debouncer = Just { var, fiber } }

      Just ({ var, fiber }) -> unit <$ do
        _ <- H.liftAff $ killFiber (error "Debounce restarted") fiber
        fiber' <- H.liftAff $ forkAff do
          delay st.debounceTime
          AVar.put str var

        H.modify_ _ { debouncer = Just { var, fiber: fiber' }}

handleQuery :: forall m a.
  MonadAff m =>
  Query a ->
  H.HalogenM State Action () Message m (Maybe a)
handleQuery = case _ of
  -- For when there is an existing search performed, but you need to set the
  -- field's text anyway.
  SetText str a -> do
    H.modify_ _ { query = str }
    openIfHasQuery str
    pure $ Just a

openIfHasQuery :: forall m.
  MonadState State m =>
  String ->
  m Unit
openIfHasQuery q =
  if null q then pure unit else H.modify_ _ { open = true }

closeIfNullQuery :: forall m.
  MonadState State m =>
  String ->
  m Unit
closeIfNullQuery q = do
  if null q then H.modify_ \st -> st { open = st.keepOpen } else pure unit

render :: forall m.
  MonadAff m =>
  State ->
  H.ComponentHTML Action () m
render st@{ query, open } =
  HH.label
    [ HP.classes $ containerClasses <> containerCondClasses
    , HE.onClick \_ -> Open
    ]
    [ HH.div
      [ HP.classes $ iconClasses <> iconCondClasses ]
      [ Icon.search_ ]
    , HH.div
      [ css "flex-grow" ]
      [ HH.input
        [ HE.onValueInput Search
        , HP.placeholder "Search"
        , HP.value query
        , HP.classes $ inputClasses <> inputCondClasses
        , HE.onBlur \_ -> Blur
        , HP.tabIndex 0
        ]
      ]
    , HH.button
      [ HE.onClick Clear
      , HP.type_ HP.ButtonButton
      , HP.classes $ buttonClasses <> buttonCondClasses <> hideClearClasses
      ]
      [ Icon.delete_ ]
    ]
   where
     containerClasses = HH.ClassName <$>
       [ "flex"
       , "no-outline"
       , "items-stretch"
       , "transition-1/4"
       , "border-b-2"
       , "group"
       ]

     containerCondClasses =
       ifOpen
         [ "max-w-160", "border-blue-88" ]
         [ "max-w-12", "border-transparent", "cursor-pointer" ]

     iconClasses = HH.ClassName <$>
       [ "pr-3"
       , "text-2xl"
       , "group-hover:text-grey-50"
       , "transition-1/4"
       ]

     iconCondClasses =
       ifOpen
         [ "text-grey-50", "mb-0", "mt-0" ]
         [ "text-grey-70", "-mb-1", "mt-1" ]

     inputClasses = HH.ClassName <$>
       [ "no-outline"
       , "flex-1"
       , "bg-transparent"
       , "h-full"
       , "transition-1/4"
       ]

     inputCondClasses =
       ifOpen
         [ "w-full" ]
         [ "w-0" ]

     buttonClasses = HH.ClassName <$>
       [ "no-outline"
       , "text-grey-70"
       , "hover:text-grey-50"
       , "text-xs"
       , "transition-1/4"
       , "flex-shrink"
       ]

     buttonCondClasses =
       ifOpen
         [ "opacity-100", "visible" ]
         [ "opacity-0", "invisible" ]

     hideClearClasses = HH.ClassName <$>
       if Data.String.null st.query then ["hidden"] else []

     ifOpen openClasses closedClasses =
       HH.ClassName <$> if open then openClasses else closedClasses
