module UIGuide.Component.Dialogs where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (delay)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Ocelot.Block.Card (card) as Card
import Ocelot.Block.FormField (field) as FormField
import Ocelot.Block.Format as Format
import Ocelot.Block.Icon as Icon
import Ocelot.Block.Input (input) as Input
import Ocelot.Block.Toast as Toast
import Ocelot.Block.Tooltip (tooltip_) as Tooltip
import Ocelot.Button as Button
import Ocelot.HTML.Properties (css)
import UIGuide.Block.Backdrop as Backdrop
import UIGuide.Block.Documentation as Documentation

type State =
  { toast :: Maybe ToastType }

data Query (a :: Type)
data Action = Toggle ToastType

data ToastType
  = Success
  | Error

--  | Info
derive instance genericToastType :: Generic ToastType _
derive instance eqToastType :: Eq ToastType
instance showToastType :: Show ToastType where
  show = genericShow

type Input = Unit

type Message = Void

component ::
  forall m.
  MonadAff m =>
  H.Component Query Input Message m
component =
  H.mkComponent
    { initialState: const { toast: Nothing }
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

  where
  handleAction :: Action -> H.HalogenM State Action () Message m Unit
  handleAction = case _ of
    Toggle t -> do
      H.modify_ _ { toast = Just t }
      H.liftAff $ delay $ Milliseconds 3000.0
      H.modify_ _ { toast = Nothing }

  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div_
      [ Documentation.block_
          { header: "Tooltips"
          , subheader: "Point out and explain UI"
          }
          [ Backdrop.backdrop_
              [ Backdrop.content
                  [ css "mt-0" ]
                  [ Card.card
                      [ css "w-1/2 m-auto" ]
                      [ FormField.field
                          { helpText: [ HH.text "The function of this field is not clear." ]
                          , label: HH.div_
                              [ HH.text "Account Id"
                              , Tooltip.tooltip_
                                  "This tooltip can explain more about what Account Id does"
                                  (Icon.info [ css "ml-2" ])
                              ]
                          , error: []
                          , inputId: "tooltip-input"
                          }
                          [ css "mb-6 pt-3" ]
                          [ Input.input [] ]
                      ]
                  ]
              ]
          ]
      , Documentation.block_
          { header: "Toasts"
          , subheader: "Less prominent dialogs that pop up from the bottom and then fall away. Usually indicate a status change. May contain actions."
          }
          [ Backdrop.backdrop_
              [ Backdrop.content_
                  [ HH.div
                      [ css "mb-6" ]
                      [ Format.caption_
                          [ HH.text "Success" ]
                      , Button.button
                          [ HE.onClick \_ -> Toggle Success ]
                          [ HH.text "Success" ]
                      , Toast.toast
                          [ Toast.visible $ state.toast == Just Success ]
                          [ Icon.success
                              [ css "text-green text-2xl mr-2" ]
                          , HH.p_
                              [ HH.text "Campaign saved." ]
                          ]
                      ]
                  ]
              ]
          , Backdrop.backdrop_
              [ Backdrop.content_
                  [ HH.div
                      [ css "mb-6" ]
                      [ Format.caption_
                          [ HH.text "Error" ]
                      , Button.button
                          [ HE.onClick \_ -> Toggle Error ]
                          [ HH.text "Error" ]
                      , Toast.toast
                          [ Toast.visible $ state.toast == Just Error ]
                          [ Icon.error
                              [ css "text-red text-2xl mr-2" ]
                          , HH.p_
                              [ HH.text "Fix errors before saving." ]
                          ]
                      ]
                  ]
              ]
          , Backdrop.backdrop_
              [ Backdrop.content_
                  [ HH.div
                      [ css "mb-6" ]
                      [ Format.caption_
                          [ HH.text "Info" ]
                      ]
                  ]
              ]
          ]
      ]
