-- ATTRIBUTION:
-- This module adapts from rnons' Halogen Storybook
-- https://github.com/rnons/purescript-halogen-storybook/
-- to fit with our UI guide branding needs

module UIGuide.App
  ( Stories
  , StoryQuery
  , runStorybook
  , module Halogen.Storybook.Proxy
  ) where

import Prelude

import Control.Monad.Aff (Aff, launchAff_)

import Data.Const (Const)
import Data.Functor (mapFlipped)
import Data.Maybe (Maybe(..))
import Data.StrMap as SM
import DOM.HTML.Types (HTMLElement)

import Global (decodeURI, encodeURI)

import Halogen as H
import Halogen.Aff (HalogenEffects)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Storybook.Proxy (ProxyS, proxy)
import Halogen.VDom.Driver (runUI)

import Routing (hashes)

data Query a
  = RouteChange String a

type State =
  { route :: String
  }

type StoryQuery = ProxyS (Const Void) Unit

-- | Stories config, each story consists of a story name and a component.
-- | Note the component needs to be proxied explicitly.
-- |
-- | ```
-- | stories :: forall m. Stories m
-- | stories = SM.fromFoldable
-- |   [ Tuple "count" $ proxy $ ExpCount.component
-- | ```
type Stories m = SM.StrMap (H.Component HH.HTML StoryQuery Unit Void m)

type Slot = String

type HTML m = H.ParentHTML Query StoryQuery Slot m


-- | Takes stories config and mount element, and renders the storybook.
runStorybook
  :: forall eff. Stories (Aff (HalogenEffects eff))
  -> HTMLElement
  -> Aff (HalogenEffects eff) Unit
runStorybook stories body = do
  app' <- runUI (app stories) unit body
  H.liftEff $ hashes $ \_ next ->
    launchAff_ $ app'.query (H.action $ RouteChange $ decodeURI next)


app :: forall m. Stories m -> H.Component HH.HTML Query Unit Void m
app stories =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = { route: "" }

  render :: State -> HTML m
  render state =
    HH.body
    [ HP.class_ $ HH.ClassName "font-sans font-normal text-black leading-normal" ]
    [ HH.div
      [ HP.class_ $ HH.ClassName "min-h-screen" ]
      [ renderSidebar state
      , renderContainer state
      ]
    ]

  renderContainer :: State -> HTML m
  renderContainer state =
    HH.div
    [ HP.class_ $ HH.ClassName "md:ml-80" ]
    [ HH.div
      [ HP.class_ $ HH.ClassName "fixed w-full z-20" ]
      [ HH.div
        [ HP.class_ $ HH.ClassName "pin-t bg-white md:hidden relative border-b border-grey-light h-12 py-8 flex items-center" ]
        [ HH.a
          [ HP.class_ $ HH.ClassName "mx-auto inline-flex items-center"
          , HP.href "" ]
          [ HH.text "CitizenNet UI Guide" ]
        ]
      ]
    , HH.div
      [ HP.class_ $ HH.ClassName "px-6 pb-8 pt-20 md:pt-16 w-full max-w-lg mx-auto" ]
      [ renderSlot state ]
    ]

  renderSlot :: State -> HTML m
  renderSlot state =
    case SM.lookup state.route stories of
      Just cmp -> HH.slot state.route cmp unit absurd
      -- TODO: Fill in a home page HTML renderer
      _ -> HH.div_ []

  renderSidebar :: State -> HTML m
  renderSidebar state =
    HH.div
    [ HP.id_ "sidebar"
    , HP.class_ $ HH.ClassName "hidden z-50 fixed pin-y pin-l overflow-y-scroll md:overflow-visible scrolling-touch md:scrolling-auto bg-grey-95 w-4/5 md:w-full md:max-w-xs flex-none border-r-2 border-grey-light md:flex flex-col" ]
    [ HH.div
      [ HP.class_ $ HH.ClassName "p-8 flex-1 overflow-y-scroll" ]
      [ HH.nav
        [ HP.class_ $ HH.ClassName "text-base overflow-y-scroll" ]
        [ renderNavs state ]
      ]
    ]

  renderNavs :: State -> HTML m
  renderNavs { route } =
    HH.ul [ HP.class_ $ HH.ClassName "list-reset" ] $
      mapFlipped (SM.keys stories) $ \name ->
        HH.li
        [ HP.class_ $ HH.ClassName "mb-3" ]
        [ HH.a
          [ HP.class_ $ HH.ClassName $ if route == name then linkActiveClass else linkClass
          , HP.href $ "#" <> encodeURI name
          ]
          [ HH.text name ]
        ]
    where
      linkClass = "hover:underline text-grey-darkest"
      linkActiveClass = linkClass <> " font-bold text-black"


  eval :: Query ~> H.ParentDSL State Query StoryQuery Slot Void m
  eval (RouteChange route next) = do
    H.modify (\state -> state { route = route })
    pure next


