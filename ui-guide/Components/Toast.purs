module UIGuide.Components.Toast where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events (input_, onClick) as HE
import Ocelot.Block.Button (button) as Button
import Ocelot.Block.Format (caption_) as Format
import Ocelot.Block.Icon as Icon
import Ocelot.Block.Toast (toast) as Toast
import Ocelot.HTML.Properties (css)
import UIGuide.Block.Backdrop as Backdrop
import UIGuide.Block.Documentation as Documentation

type State =
  { toast :: Maybe ToastType }

data Query a
  = Toggle ToastType a

data ToastType
  = Success
  | Error
  --  | Info
derive instance eqToastType :: Eq ToastType

type Input = Unit

type Message = Void

component
  :: ∀ m
   . MonadAff m
  => H.Component HH.HTML Query Input Message m
component =
  H.component
    { initialState: const { toast: Nothing }
    , render
    , eval
    , receiver: const Nothing
    }

  where
    eval :: Query ~> H.ComponentDSL State Query Message m
    eval (Toggle t a) = do
      H.modify_ _ { toast = Just t }
      pure a

    render :: State -> H.ComponentHTML Query
    render state =
      HH.div_
        [ Documentation.block_
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
                  [ HE.onClick $ HE.input_ $ Toggle Success ]
                  [ HH.text "Success" ]
                , if state.toast == Just Success
                    then
                      Toast.toast
                      []
                      [ Icon.success
                        [ css "text-green text-2xl mr-2" ]
                      , HH.p_
                        [ HH.text "Campaign saved." ]
                      ]
                    else HH.text ""
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
                  [ HE.onClick $ HE.input_ $ Toggle Error ]
                  [ HH.text "Error" ]
                , if state.toast == Just Error
                    then
                      Toast.toast
                      []
                      [ Icon.error
                        [ css "text-red text-2xl mr-2" ]
                      , HH.p_
                        [ HH.text "Fix errors before saving." ]
                      ]
                    else HH.text ""
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
