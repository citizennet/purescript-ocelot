-- ATTRIBUTION:
-- This module adapts from rnons' Halogen Storybook
-- https://github.com/rnons/purescript-halogen-storybook/
-- to fit with our UI guide branding needs

module UIGuide.App
  ( Stories
  , StoryQuery
  , Page(..)
  , Group(..)
  , runStorybook
  , module Halogen.Storybook.Proxy
  ) where

import Prelude

import Effect.Aff (Aff, launchAff_)

import Data.Tuple (Tuple(..))
import Data.Const (Const)
import Data.Functor (mapFlipped)
import Data.Maybe (Maybe(..))
import Data.Map as M
import Web.HTML.HTMLElement (HTMLElement)

import Global.Unsafe (unsafeDecodeURI, unsafeEncodeURI)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Storybook.Proxy (ProxyS, proxy)
import Halogen.VDom.Driver (runUI)
import Ocelot.Block.Format as Format
import UIGuide.Block.Backdrop as Backdrop

import Routing.Hash (hashes)

data Query a
  = RouteChange String a

type State m =
  { route :: String
  , stories :: Stories m
  , partitions :: M.Map Group (Stories m)
  }

type StoryQuery = ProxyS (Const Void) Unit

type Stories m = M.Map String (Page m)

type Page m =
  { anchor :: String
  , component :: H.Component HH.HTML StoryQuery Unit Void m
  , group :: Group
  }

data Group
  = Basics
  | FormElements
  | Components

derive instance eqGroup :: Eq Group
derive instance ordGroup :: Ord Group
instance showGroup :: Show Group where
  show Basics = "Basics"
  show FormElements = "Form Elements"
  show Components = "Components"


type Slot = String

type HTML m = H.ParentHTML Query StoryQuery Slot m


-- | Takes stories config and mount element, and renders the storybook.
runStorybook
 :: Stories Aff
 -> Array Group
 -> HTMLElement
 -> Aff Unit
runStorybook stories groups body = do
  app' <- runUI app { stories, groups } body
  void $ H.liftEffect $ hashes $ \_ next ->
    launchAff_ $ app'.query (H.action $ RouteChange $ unsafeDecodeURI next)

type Input m =
  { stories :: Stories m
  , groups :: Array Group
  }

app :: ∀ m. H.Component HH.HTML Query (Input m) Void m
app =
  H.parentComponent
    { initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: Input m -> State m
  initialState i = { route: "", stories: i.stories, partitions: M.fromFoldable $ flip partitionByGroup i.stories <$> i.groups }

  render :: State m -> HTML m
  render state =
    HH.body_
    [ HH.div
      [ HP.class_ $ HH.ClassName "min-h-screen" ]
      [ renderSidebar state
      , renderContainer state
      ]
    ]

  renderContainer :: State m -> HTML m
  renderContainer state =
    HH.div
    [ HP.class_ $ HH.ClassName "md:ml-80" ]
    [ HH.div
      [ HP.class_ $ HH.ClassName "fixed w-full" ]
      [ HH.div
        [ HP.class_ $ HH.ClassName "pin-t bg-white md:hidden relative border-b border-grey-light h-12 py-8 flex items-center" ]
        [ HH.a
          [ HP.class_ $ HH.ClassName "mx-auto inline-flex items-center"
          , HP.href "" ]
          [ HH.text "CitizenNet UI Guide" ]
        ]
      ]
    , HH.div
      [ HP.class_ $ HH.ClassName "p-12 w-full container mx-auto" ]
      [ renderSlot state ]
    ]

  renderSlot :: State m -> HTML m
  renderSlot state =
    case M.lookup state.route state.stories of
      Just { component } -> HH.slot state.route component unit absurd
      -- TODO: Fill in a home page HTML renderer
      _ -> HH.div_ []

  renderSidebar :: State m -> HTML m
  renderSidebar state =
    Backdrop.backdrop
    [ HP.id_ "sidebar"
    , HP.classes
      ( HH.ClassName <$>
        [ "hidden"
        , "fixed"
        , "pin-y"
        , "pin-l"
        , "overflow-y-auto"
        , "md:overflow-visible"
        , "scrolling-touch"
        , "md:scrolling-auto"
        , "w-4/5"
        , "md:w-full"
        , "md:max-w-xs"
        , "flex-none"
        -- , "border-r-2"
        -- , "border-grey-light"
        , "md:flex"
        , "flex-col"
        ]
      )
    ]
    [ HH.div
      [ HP.class_ $ HH.ClassName "flex-1 p-6 overflow-y-auto" ]
      [ HH.header_
        [ Format.heading
          [ HP.class_ $ HH.ClassName "flex" ]
          [ HH.img
            [ HP.class_ $ HH.ClassName "mr-2"
            , HP.src "https://citizennet.com/manager/images/logo.svg"
            ]
          , HH.text "Ocelot"
          ]
        ]
      , HH.nav
        [ HP.class_ $ HH.ClassName "text-base overflow-y-auto" ]
        (renderGroups state)
      ]
    ]

  renderGroups :: State m -> Array (HTML m)
  renderGroups state =
    mapFlipped (M.toUnfoldable state.partitions) $ \(Tuple group stories) ->
      HH.div
      [ HP.class_ $ HH.ClassName "mb-6" ]
      [ Format.caption_
        [ HH.text $ show group ]
      , renderGroup state.route stories
      ]

  renderGroup :: String -> Stories m -> HTML m
  renderGroup route stories =
    HH.ul [ HP.class_ $ HH.ClassName "list-reset" ] $
      mapFlipped (M.toUnfoldable stories) $ \(Tuple href { anchor }) ->
        HH.li
        [ HP.class_ $ HH.ClassName "mb-3" ]
        [ HH.a
          [ HP.classes $
            Format.linkClasses <>
            ( if href == route then [ HH.ClassName "font-medium" ] else [] )
          , HP.href $ "#" <> unsafeEncodeURI href
          ]
          [ HH.text anchor ]
        ]


  eval :: Query ~> H.ParentDSL (State m) Query StoryQuery Slot Void m
  eval (RouteChange route next) = do
    H.modify_ (\state -> state { route = route })
    pure next





----------
-- Helpers

partitionByGroup :: ∀ m. Group -> Stories m -> Tuple Group (Stories m)
partitionByGroup g = Tuple g <<< M.filter (\{ group } -> group == g)
