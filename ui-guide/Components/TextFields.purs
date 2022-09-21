module UIGuide.Component.TextFields where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ocelot.Block.Card as Card
import Ocelot.Block.FormField as FormField
import Ocelot.Block.Format as Format
import Ocelot.Block.Icon as Icon
import Ocelot.Block.Input as Input
import Ocelot.Block.Loading as Loading
import Ocelot.Button as Button
import Ocelot.Component.SearchBar as SearchBar
import Ocelot.HTML.Properties (css)
import Type.Proxy (Proxy(..))
import UIGuide.Block.Backdrop as Backdrop
import UIGuide.Block.Documentation as Documentation

type State = Unit

data Query (a :: Type)
data Action = HandleSearch SearchBar.Message

type Input = Unit

type Message = Void

type ChildSlot =
  ( search :: SearchBar.Slot Unit )

_search = Proxy :: Proxy "search"

type ChildQuery = SearchBar.Query

component :: ∀ m. MonadAff m => H.Component Query Input Message m
component =
  H.mkComponent
    { initialState: const unit
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
    handleAction :: Action -> H.HalogenM State Action ChildSlot Message m Unit
    handleAction = case _ of
      HandleSearch (SearchBar.Searched str) -> do
        void $ H.liftEffect $ log str

    render :: State -> H.ComponentHTML Action ChildSlot m
    render _ = cnDocumentationBlocks


----------
-- HTML

cnDocumentationBlocks :: ∀ m. MonadAff m => H.ComponentHTML Action ChildSlot m
cnDocumentationBlocks =
  let content = Backdrop.content [ css "flex" ] in
  HH.div_
  [ Documentation.block_
    { header: "Text Fields"
    , subheader: "Captures string input."
    }
    [ Backdrop.backdrop_
      [ content
        [ Card.card
          [ HP.class_ $ HH.ClassName "flex-1" ]
          [ HH.h3
            [ HP.classes Format.captionClasses ]
            [ HH.text "Static" ]
          , FormField.field_
            { label: HH.text "Email*"
            , helpText: [ HH.text "Add the email of the End Advertiser." ]
            , error: []
            , inputId: "email"
            }
            [ Input.input
              [ HP.placeholder "address@gmail.com"
              , HP.id "email"
              ]
            ]
          , HH.h3
            [ HP.classes Format.captionClasses ]
            [ HH.text "Error" ]
          , FormField.field_
            { label: HH.text "Email*"
            , helpText: [ HH.text "Add the email of the End Advertiser." ]
            , error:
              [ HH.ul_ $
                HH.li_ <<< pure <<< HH.text <$> [ "This field is required", "Must be a valid email" ]
              ]
            , inputId: "email-error"
            }
            [ Input.input
              [ HP.placeholder "address@gmail.com"
              , HP.id "email-error"
              ]
            ]
          ]
        ]
      , content
        [ Card.card
          [ HP.class_ $ HH.ClassName "flex-1" ]
          [ HH.h3
            [ HP.classes Format.captionClasses ]
            [ HH.text "Hydrated" ]
          , FormField.field_
            { label: HH.text "Email*"
            , helpText: [ HH.text "Add the email of the End Advertiser." ]
            , error: []
            , inputId: "email-hydrated"
            }
            [ Input.input
              [ HP.value "jeff@citizennet.com"
              , HP.id "email-hydrated"
              ]
            ]
          , HH.h3
            [ HP.classes Format.captionClasses ]
            [ HH.text "Disabled" ]
          , FormField.field_
            { label: HH.text "Email*"
            , helpText: [ HH.text "Add the email of the End Advertiser." ]
            , error: []
            , inputId: "email-disabled"
            }
            [ Input.input
              [ HP.value "jeff@citizennet.com"
              , HP.id "email-disabled"
              , HP.disabled true
              ]
            ]
          ]
        ]
      ]
    ]
  , Documentation.block_
    { header: "Text Fields - Right Addon"
    , subheader: "Captures string input while indicating to user useful information about the input type."
    }
    [ Backdrop.backdrop_
      [ content
        [ Card.card
          [ HP.class_ $ HH.ClassName "flex-1" ]
          [ HH.h3
            [ HP.classes Format.captionClasses ]
            [ HH.text "Static" ]
          , FormField.fieldSmall_
            { label: HH.text "Daily Goal"
            , helpText: [ HH.text "Desired daily spend as percentage of total budget." ]
            , error: []
            , inputId: "daily-goal"
            }
            [ Input.percentage_
              [ HP.id "daily-goal"
              ]
            ]
          , HH.h3
            [ HP.classes Format.captionClasses ]
            [ HH.text "Error" ]
          , FormField.fieldSmall_
            { label: HH.text "Daily Goal"
            , helpText: [ HH.text "Desired daily spend as percentage of total budget." ]
            , error: [ HH.text "Must be between 0 and 100" ]
            , inputId: "daily-goal-error"
            }
            [ Input.percentage_
              [ HP.value "200"
              , HP.id "daily-goal-error"
              ]
            ]
          ]
        ]
      , content
        [ Card.card
          [ HP.class_ $ HH.ClassName "flex-1" ]
          [ HH.h3
            [ HP.classes Format.captionClasses ]
            [ HH.text "Hydrated" ]
          , FormField.fieldSmall_
            { label: HH.text "Daily Goal"
            , helpText: [ HH.text "Desired daily spend as percentage of total budget." ]
            , error: []
            , inputId: "daily-goal-hydrated"
            }
            [ Input.percentage_
              [ HP.value "25"
              , HP.id "daily-goal-hydrated"
              ]
            ]
          , HH.h3
            [ HP.classes Format.captionClasses ]
            [ HH.text "Disabled" ]
          , FormField.fieldSmall_
            { label: HH.text "Daily Goal"
            , helpText: [ HH.text "Desired daily spend as percentage of total budget." ]
            , error: []
            , inputId: "daily-goal-disabled"
            }
            [ Input.percentage_
              [ HP.value "25"
              , HP.id "daily-goal-disabled"
              , HP.disabled true
              ]
            ]
          ]
        ]
      ]
    ]
  , Documentation.block_
    { header: "Text Fields - Left Addon"
    , subheader: "Captures string input while indicating to user useful information about the input type."
    }
    [ Backdrop.backdrop_
      [ content
        [ Card.card
          [ HP.class_ $ HH.ClassName "flex-1" ]
          [ HH.h3
            [ HP.classes Format.captionClasses ]
            [ HH.text "Static" ]
          , FormField.fieldSmall_
            { label: HH.text "Budget*"
            , helpText: [ HH.text "Total amount for campaign to spend." ]
            , error: []
            , inputId: "budget"
            }
            [ Input.currency_
              [ HP.id "budget"
              ]
            ]
          , HH.h3
            [ HP.classes Format.captionClasses ]
            [ HH.text "Error" ]
          , FormField.fieldSmall_
            { label: HH.text "Budget*"
            , helpText: [ HH.text "Total amount for campaign to spend." ]
            , error: [ HH.text "This field is required." ]
            , inputId: "budget-error"
            }
            [ Input.currency_
              [ HP.id "budget-error"
              ]
            ]
          ]
        ]
      , content
        [ Card.card
          [ HP.class_ $ HH.ClassName "flex-1" ]
          [ HH.h3
            [ HP.classes Format.captionClasses ]
            [ HH.text "Hydrated" ]
          , FormField.fieldSmall_
            { label: HH.text "Budget*"
            , helpText: [ HH.text "Total amount for campaign to spend." ]
            , error: []
            , inputId: "budget-hydrated"
            }
            [ Input.currency_
              [ HP.value "50,000"
              , HP.id "budget-hydrated"
              ]
            ]
          , HH.h3
            [ HP.classes Format.captionClasses ]
            [ HH.text "Disabled" ]
          , FormField.fieldSmall_
            { label: HH.text "Budget*"
            , helpText: [ HH.text "Total amount for campaign to spend." ]
            , error: []
            , inputId: "budget-disabled"
            }
            [ Input.currency_
              [ HP.value "50,000"
              , HP.id "budget-disabled"
              , HP.disabled true
              ]
            ]
          ]
        ]
      ]
    ]
  , Documentation.block_
    { header: "Text Fields - Surrounding Addons"
    , subheader: "Captures string input while indicating to user useful information about the input type."
    }
    [ Backdrop.backdrop_
      [ content
        [ Card.card
          [ HP.class_ $ HH.ClassName "flex-1" ]
          [ HH.h3
            [ HP.classes Format.captionClasses ]
            [ HH.text "Static" ]
          , FormField.field_
            { label: HH.text "Search"
            , helpText: [ HH.text "This text field shows how you might represent a search field." ]
            , error: []
            , inputId: "search"
            }
            [ Input.inputGroup_
              [ Input.inputCenter
                [ HP.id "search"
                , HP.class_ $ HH.ClassName "focus:next:text-blue-88"
                ]
              , Input.addonLeft_ [ Icon.search_ ]
              , Input.borderRight
                [ HP.classes Format.linkClasses ]
                [ HH.text "Search" ]
              ]
            ]
          ]
        ]
      , content
        [ Card.card
          [ HP.class_ $ HH.ClassName "flex-1" ]
          [ HH.h3
            [ HP.classes Format.captionClasses ]
            [ HH.text "Loading" ]
          , FormField.field_
            { label: HH.text "Search"
            , helpText: [ HH.text "This text field shows how you might represent a loading state for a search field." ]
            , error: []
            , inputId: "search-loading"
            }
            [ Input.inputGroup_
              [ Input.inputCenter
                [ HP.class_ $ HH.ClassName "focus:next:text-blue-88"
                , HP.id "search-loading"
                , HP.value "Something"
                ]
              , Input.addonCenter_ [ Loading.spinner [ HP.class_ $ HH.ClassName "w-6 text-blue-88" ] ]
              , Input.addonLeft_ [ Icon.search_ ]
              , Input.borderRight
                [ HP.classes Format.linkClasses ]
                [ HH.text "Search" ]
              ]
            ]
          ]
        ]
      ]
    ]
  , Documentation.block_
    { header: "Text Areas"
    , subheader: "Captures large string input."
    }
    [ Backdrop.backdrop_
      [ content
        [ Card.card
          [ HP.class_ $ HH.ClassName "flex-1" ]
          [ HH.h3
            [ HP.classes Format.captionClasses ]
            [ HH.text "Static" ]
          , FormField.field_
            { label: HH.text "Comment"
            , helpText: [ HH.text "Say something." ]
            , error: []
            , inputId: "comment"
            }
            [ Input.textarea
              [ HP.placeholder "Be nice."
              , HP.id "comment"
              ]
            ]
          , HH.h3
            [ HP.classes Format.captionClasses ]
            [ HH.text "Error" ]
          , FormField.field_
            { label: HH.text "Comment*"
            , helpText: [ HH.text "Say something." ]
            , error: [ HH.text "This field is required." ]
            , inputId: "comment-error"
            }
            [ Input.textarea
              [ HP.placeholder "Be nice."
              , HP.id "comment-error"
              ]
            ]
          ]
        ]
      , content
        [ Card.card
          [ HP.class_ $ HH.ClassName "flex-1" ]
          [ HH.h3
            [ HP.classes Format.captionClasses ]
            [ HH.text "Hydrated" ]
          , FormField.field_
            { label: HH.text "Comment"
            , helpText: [ HH.text "Say something." ]
            , error: []
            , inputId: "comment-hydrated"
            }
            [ Input.textarea
              [ HP.value "Forest drinks on the job"
              , HP.id "comment-hydrated"
              ]
            ]
          , HH.h3
            [ HP.classes Format.captionClasses ]
            [ HH.text "Disabled" ]
          , FormField.field_
            { label: HH.text "Comment*"
            , helpText: [ HH.text "Say something." ]
            , error: []
            , inputId: "comment-disabled"
            }
            [ Input.textarea
              [ HP.value "Forest drinks on the job"
              , HP.id "comment-disabled"
              , HP.disabled true
              ]
            ]
          ]
        ]
      ]
    ]
  , Documentation.block_
    { header: "Search Bar"
    , subheader: "A component for handling searching"
    }
    [ Backdrop.backdrop_
      [ content
        [ Card.card
          [ HP.class_ $ HH.ClassName "flex-1" ]
          [ HH.h3
            [ HP.classes Format.captionClasses ]
            [ HH.text "Standard" ]
          , HH.div
            [ css "w-50" ]
            [ HH.slot _search unit SearchBar.component
              { debounceTime: Just (Milliseconds 250.0) }
              HandleSearch
            ]
          ]
        ]
      , content
        [ Card.card
          [ HP.class_ $ HH.ClassName "flex-1" ]
          [ HH.h3
            [ HP.classes Format.captionClasses ]
            [ HH.text "Keep Open" ]
          , HH.div
            [ css "w-50" ]
            [ HH.slot _search unit SearchBar.component'
              { debounceTime: Just (Milliseconds 250.0)
              , keepOpen: true
              }
              HandleSearch
            ]
          ]
        ]
      ]
    ]
  , Documentation.block_
    { header: "Search Bar - With Neighbors"
    , subheader: "A component for handling search, sharing a space with other blocks"
    }
    [ Backdrop.backdrop_
      [ Backdrop.content_
        [ HH.div
          [ css "flex items-center pb-6" ]
          [ HH.div
            [ css "mr-8" ]
            [ HH.slot _search unit SearchBar.component
              { debounceTime: Just (Milliseconds 250.0) }
              HandleSearch
            ]
          , Button.buttonPrimary_
            [ HH.text "Neighbor" ]
          ]
        ]
      ]
    ]
  ]
