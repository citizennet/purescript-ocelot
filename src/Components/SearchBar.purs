module Ocelot.Component.SearchBar where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (null)
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
  }

type Debouncer =
  { var :: AVar String
  , fiber :: Fiber Unit
  }

data Query a
  = Clear ME.MouseEvent a
  | Search String a
  | SetText String a
  | Open a
  | Blur a

type Input = { debounceTime :: Maybe Milliseconds }

data Message
 = Searched String

component :: ∀ m. MonadAff m => H.Component HH.HTML Query Input Message m
component =
  H.component
    { initialState
    , eval
    , render
    , receiver: const Nothing
    }

  where
    initialState :: Input -> State
    initialState { debounceTime } =
      { query: ""
      , debouncer: Nothing
      , debounceTime: fromMaybe (Milliseconds 0.0) debounceTime
      , open: false
      }

    eval :: Query ~> H.ComponentDSL State Query Message m
    eval = case _ of
      Open a -> do
        H.modify_ _ { open = true }
        pure a

      Blur a -> do
        query <- H.gets _.query
        closeIfNullQuery query
        pure a

      Clear ev a -> do
        H.liftEffect $ stopPropagation $ ME.toEvent ev
        H.modify_ _ { query = "", open = false }
        H.raise $ Searched ""
        pure a

      -- For when there is an existing search performed, but you need to set the
      -- field's text anyway.
      SetText str a -> do
        H.modify_ _ { query = str }
        openIfHasQuery str
        pure a

      Search str a -> do
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

        pure a

      where
      openIfHasQuery q =
        if null q then pure unit else H.modify_ _ { open = true }

      closeIfNullQuery q =
        if null q then H.modify_ _ { open = false } else pure unit

    render :: State -> H.ComponentHTML Query
    render { query, open } =
      HH.label
        [ HP.classes $ containerClasses <> containerCondClasses
        , HE.onClick (HE.input $ const Open)
        ]
        [ HH.div
          [ HP.classes $ iconClasses <> iconCondClasses ]
          [ Icon.search_ ]
        , HH.div
          [ css "flex-grow" ]
          [ HH.input
            [ HE.onValueInput (HE.input Search)
            , HP.placeholder "Search"
            , HP.value query
            , HP.classes $ inputClasses <> inputCondClasses
            , HE.onBlur (HE.input $ const Blur)
            , HP.tabIndex 0
            ]
          ]
        , HH.button
          [ HE.onClick (\ev -> Just $ Clear ev unit)
          , HP.type_ HP.ButtonButton
          , HP.classes $ buttonClasses <> buttonCondClasses
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

        ifOpen openClasses closedClasses =
          HH.ClassName <$> if open then openClasses else closedClasses
