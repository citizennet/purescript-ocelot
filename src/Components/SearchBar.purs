module Ocelot.Components.SearchBar
  ( component
  , Query
  , Message(..)
  )
  where

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
import Ocelot.Block.Icon (close_, search_) as Icon

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
  = Clear a
  | Search String a
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
        if null query then H.modify_ _ { open = false } else pure unit
        pure a

      Clear a -> do
        H.modify_ _ { query = "" }
        H.modify_ _ { open = false }
        H.raise $ Searched ""
        pure a

      Search str a -> do
        H.modify_ _ { query = str }
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

    render :: State -> H.ComponentHTML Query
    render { query, open } =
      HH.label
        [ HP.classes $ containerClasses <> containerCondClasses ]
        [ HH.input
          [ HE.onValueInput (HE.input Search)
          , HP.placeholder "Search"
          , HP.value query
          , HP.classes $ inputClasses <> inputCondClasses
          , HE.onBlur (HE.input $ const Blur)
          ]
        , HH.div
          [ HP.classes $ labelClasses <> labelCondClasses
          , HE.onClick (HE.input $ const Open)
          ]
          [ Icon.search_ ]
        , HH.button
          [ HE.onClick (HE.input $ const Clear)
          , HP.type_ (HP.ButtonButton)
          , HP.classes $ buttonClasses <> buttonCondClasses
          ]
          [ Icon.close_ ]
        ]

      where
        containerClasses = HH.ClassName <$>
          [ "flex"
          , "border-b-2"
          , "border-blue-88"
          , "no-outline"
          , "items-stretch"
          , "w-0"
          , "transition-1/4"
          ]

        containerCondClasses = ifOpen [ "w-full" ] [ ]

        labelClasses = HH.ClassName <$>
          [ "mr-3"
          , "text-2xl"
          , "order--1"
          , "cursor-pointer"
          ]

        labelCondClasses = ifOpen [ "text-grey-50" ] [ "text-grey-70" ]

        inputClasses = HH.ClassName <$>
          [ "no-outline"
          , "flex-1"
          , "bg-transparent"
          , "w-0"
          ]

        inputCondClasses = ifOpen [ "w-full" ] [ ]

        buttonClasses = HH.ClassName <$>
          [ "no-outline"
          , "text-grey-80"
          , "hover:text-grey-70"
          , "text-xs"
          , "transition-1/4"
          ]

        buttonCondClasses = ifOpen [ "opacity-100",  "visible" ] [ "opacity-0", "invisible" ]

        ifOpen openClasses closedClasses =
          HH.ClassName <$> if open then openClasses else closedClasses
