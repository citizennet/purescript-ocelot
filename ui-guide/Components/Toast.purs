module UIGuide.Components.Toast where

import Prelude

import Data.Array (mapWithIndex)
import Data.Either.Nested (Either2)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Ocelot.Block.Button as Button
import Ocelot.Block.Format (caption_) as Format
import Ocelot.Block.Icon as Icon
import Ocelot.Blocks.Choice as Choice
import Ocelot.Components.Dropdown as Dropdown
import Ocelot.HTML.Properties (css)
import UIGuide.Block.Backdrop as Backdrop
import UIGuide.Block.Documentation as Documentation

type State = Unit

data Query a

type Input = Unit

type Message = Void

component
  :: ∀ m
   . MonadAff m
  => H.Component HH.HTML Query Input Message m
component =
  H.component
    { initialState: const unit
    , render
    , eval
    , receiver: const Nothing
    }

  where
    eval :: Query ~> H.ComponentDSL State Query Message m
    eval = const $ pure unit

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
                ]
              ]
            ]
          , Backdrop.backdrop_
            [ Backdrop.content_
              [ HH.div
                [ css "mb-6" ]
                [ Format.caption_
                  [ HH.text "Error" ]
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
