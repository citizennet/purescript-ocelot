module UIGuide.Components.TextFields where

import Prelude

import Ocelot.Block.Card as Card
import Ocelot.Block.FormField as FormField
import Ocelot.Block.Icon as Icon
import Ocelot.Block.Input as Input
import Ocelot.Block.Type as Type
import Ocelot.Components.Typeahead as TA
import Ocelot.Core.Typeahead as TACore
import Ocelot.Core.Validation as Validation
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff.Timer (TIMER)
import DOM (DOM)
import Data.Either.Nested (Either2)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax (AJAX)
import UIGuide.Utilities.Async as Async
import UIGuide.Block.Documentation as Documentation
import UIGuide.Block.Backdrop as Backdrop

type State = Unit

data Query a = NoOp a

type Input = Unit

type Message = Void

component :: ∀ m. H.Component HH.HTML Query Input Message m
component =
  H.component
    { initialState: const unit
    , render
    , eval
    , receiver: const Nothing
    }
  where
    eval :: Query ~> H.ComponentDSL State Query Message m
    eval = case _ of
      NoOp a -> do
        pure a

    render :: State -> H.ComponentHTML Query
    render _ = cnDocumentationBlocks


----------
-- HTML

cnDocumentationBlocks :: H.ComponentHTML Query
cnDocumentationBlocks =
  let css :: ∀ p i. String -> H.IProp ( "class" :: String | p ) i
      css = HP.class_ <<< HH.ClassName
      content = Backdrop.content [ css "flex" ] in
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
            [ HP.classes Type.captionClasses ]
            [ HH.text "Static" ]
          , FormField.field_
            { label: "Email*"
            , helpText: Just "Add the email of the End Advertiser."
            , valid: Nothing
            , inputId: "email"
            }
            [ Input.input
              [ HP.placeholder "address@gmail.com"
              , HP.id_ "email"
              ]
            ]
          , HH.h3
            [ HP.classes Type.captionClasses ]
            [ HH.text "Error" ]
          , FormField.field_
            { label: "Email*"
            , helpText: Just "Add the email of the End Advertiser."
            , valid: Just [ Validation.EmptyField ]
            , inputId: "email-error"
            }
            [ Input.input
              [ HP.placeholder "address@gmail.com"
              , HP.id_ "email-error"
              ]
            ]
          ]
        ]
      , content
        [ Card.card
          [ HP.class_ $ HH.ClassName "flex-1" ]
          [ HH.h3
            [ HP.classes Type.captionClasses ]
            [ HH.text "Hydrated" ]
          , FormField.field_
            { label: "Email*"
            , helpText: Just "Add the email of the End Advertiser."
            , valid: Nothing
            , inputId: "email-hydrated"
            }
            [ Input.input
              [ HP.value "jeff@citizennet.com"
              , HP.id_ "email-hydrated"
              ]
            ]
          , HH.h3
            [ HP.classes Type.captionClasses ]
            [ HH.text "Disabled" ]
          , FormField.field_
            { label: "Email*"
            , helpText: Just "Add the email of the End Advertiser."
            , valid: Nothing
            , inputId: "email-disabled"
            }
            [ Input.input
              [ HP.value "jeff@citizennet.com"
              , HP.id_ "email-disabled"
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
            [ HP.classes Type.captionClasses ]
            [ HH.text "Static" ]
          , FormField.fieldSmall_
            { label: "Daily Goal"
            , helpText: Just "Desired daily spend as percentage of total budget."
            , valid: Nothing
            , inputId: "daily-goal"
            }
            [ Input.percentage_
              [ HP.id_ "daily-goal"
              ]
            ]
          , HH.h3
            [ HP.classes Type.captionClasses ]
            [ HH.text "Error" ]
          , FormField.fieldSmall_
            { label: "Daily Goal"
            , helpText: Just "Desired daily spend as percentage of total budget."
            , valid: Just [ Validation.OutOfRange "Must be between 0 and 100" ]
            , inputId: "daily-goal-error"
            }
            [ Input.percentage_
              [ HP.value "200"
              , HP.id_ "daily-goal-error"
              ]
            ]
          ]
        ]
      , content
        [ Card.card
          [ HP.class_ $ HH.ClassName "flex-1" ]
          [ HH.h3
            [ HP.classes Type.captionClasses ]
            [ HH.text "Hydrated" ]
          , FormField.fieldSmall_
            { label: "Daily Goal"
            , helpText: Just "Desired daily spend as percentage of total budget."
            , valid: Nothing
            , inputId: "daily-goal-hydrated"
            }
            [ Input.percentage_
              [ HP.value "25"
              , HP.id_ "daily-goal-hydrated"
              ]
            ]
          , HH.h3
            [ HP.classes Type.captionClasses ]
            [ HH.text "Disabled" ]
          , FormField.fieldSmall_
            { label: "Daily Goal"
            , helpText: Just "Desired daily spend as percentage of total budget."
            , valid: Nothing
            , inputId: "daily-goal-disabled"
            }
            [ Input.percentage_
              [ HP.value "25"
              , HP.id_ "daily-goal-disabled"
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
            [ HP.classes Type.captionClasses ]
            [ HH.text "Static" ]
          , FormField.fieldSmall_
            { label: "Budget*"
            , helpText: Just "Total amount for campaign to spend."
            , valid: Nothing
            , inputId: "budget"
            }
            [ Input.currency_
              [ HP.id_ "budget"
              ]
            ]
          , HH.h3
            [ HP.classes Type.captionClasses ]
            [ HH.text "Error" ]
          , FormField.fieldSmall_
            { label: "Budget*"
            , helpText: Just "Total amount for campaign to spend."
            , valid: Just [ Validation.EmptyField ]
            , inputId: "budget-error"
            }
            [ Input.currency_
              [ HP.id_ "budget-error"
              ]
            ]
          ]
        ]
      , content
        [ Card.card
          [ HP.class_ $ HH.ClassName "flex-1" ]
          [ HH.h3
            [ HP.classes Type.captionClasses ]
            [ HH.text "Hydrated" ]
          , FormField.fieldSmall_
            { label: "Budget*"
            , helpText: Just "Total amount for campaign to spend."
            , valid: Nothing
            , inputId: "budget-hydrated"
            }
            [ Input.currency_
              [ HP.value "50,000"
              , HP.id_ "budget-hydrated"
              ]
            ]
          , HH.h3
            [ HP.classes Type.captionClasses ]
            [ HH.text "Disabled" ]
          , FormField.fieldSmall_
            { label: "Budget*"
            , helpText: Just "Total amount for campaign to spend."
            , valid: Nothing
            , inputId: "budget-disabled"
            }
            [ Input.currency_
              [ HP.value "50,000"
              , HP.id_ "budget-disabled"
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
            [ HP.classes Type.captionClasses ]
            [ HH.text "Static" ]
          , FormField.field_
            { label: "Search"
            , helpText: Just "This text field shows how you might represent a search field."
            , valid: Nothing
            , inputId: "search"
            }
            [ Input.inputGroup_
              [ Input.inputCenter
                [ HP.id_ "search"
                , HP.class_ $ HH.ClassName "focus:next:text-blue-88"
                ]
              , Input.addonLeft_ [ Icon.search_ ]
              , Input.borderRight
                [ HP.classes Type.linkClasses ]
                [ HH.text "Search" ]
              ]
            ]
          ]
        ]
      , content
        [ Card.card
          [ HP.class_ $ HH.ClassName "flex-1" ]
          [ HH.h3
            [ HP.classes Type.captionClasses ]
            [ HH.text "Loading" ]
          , FormField.field_
            { label: "Search"
            , helpText: Just "This text field shows how you might represent a loading state for a search field."
            , valid: Nothing
            , inputId: "search-loading"
            }
            [ Input.inputGroup_
              [ Input.inputCenter
                [ HP.class_ $ HH.ClassName "focus:next:text-blue-88"
                , HP.id_ "search-loading"
                , HP.value "Something"
                ]
              , Input.addonCenter_ [ Icon.loading_ ]
              , Input.addonLeft_ [ Icon.search_ ]
              , Input.borderRight
                [ HP.classes Type.linkClasses ]
                [ HH.text "Search" ]
              ]
            ]
          ]
        ]
      ]
    ]]
