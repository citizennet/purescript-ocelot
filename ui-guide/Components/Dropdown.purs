module UIGuide.Components.Dropdown where

import Prelude

import Data.Either.Nested (Either2)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Ocelot.Block.Format (caption_) as Format
import Ocelot.Components.Choice as Choice
import Ocelot.Components.Dropdown as Dropdown
import Ocelot.HTML.Properties (css)
import UIGuide.Block.Backdrop as Backdrop
import UIGuide.Block.Documentation as Documentation

type State = Unit

data Query a
  = HandleDropdown (Dropdown.Message String) a
  | HandleChoice (Choice.Message) a

type Input = Unit

type Message = Void

type ChildSlot = Either2 Unit Unit

type ChildQuery = Coproduct2 (Dropdown.Query String) Choice.Query

component
  :: ∀ m
   . MonadAff m
  => H.Component HH.HTML Query Input Message m
component =
  H.parentComponent
    { initialState: const unit
    , render
    , eval
    , receiver: const Nothing
    }

  where
    eval
      :: Query
      ~> H.ParentDSL State Query ChildQuery ChildSlot Message m
    eval = case _ of
      HandleDropdown message a -> case message of
        Dropdown.ItemSelected x -> pure a

      HandleChoice message a -> case message of
        Choice.ItemSelected x -> pure a

    render
      :: State
      -> H.ParentHTML Query ChildQuery ChildSlot m
    render state =
      HH.div_
        [ Documentation.block_
          { header: "Dropdown"
          , subheader: "A dropdown list of selectable items."
          }
          [ Backdrop.backdrop
            [ css "flex" ]
            [ HH.div
              [ css "flex-1" ]
              [ Format.caption_
                [ HH.text "Standard" ]
              , HH.slot'
                CP.cp1
                unit
                Dropdown.component
                { selectedItem: Nothing
                , items
                , label: "Pick one"
                , toString: identity
                , disabled: false
                }
                ( HE.input HandleDropdown )
              ]
            ]
          , Backdrop.backdrop
            [ css "flex" ]
            [ HH.div
              [ css "flex-1" ]
              [ Format.caption_
                [ HH.text "Disabled & Hydrated" ]
              , HH.slot'
                CP.cp1
                unit
                Dropdown.component
                { selectedItem: Just "Kilchoman Blue Label"
                , items
                , label: "Pick one"
                , toString: identity
                , disabled: true
                }
                ( HE.input HandleDropdown )
              ]
            ]
          ]
        , Documentation.block_
          { header: "Choice"
          , subheader: "A specialized dropdown for making selections."
          }
          [ Backdrop.backdrop
            [ css "h-40 flex items-center justify-center" ]
            [ HH.slot'
                CP.cp2
                unit
                Choice.component
                unit
                ( HE.input HandleChoice )
            ]
          ]
        ]

      where
        items =
          [ "Lagavulin 16"
          , "Kilchoman Blue Label"
          , "Laphroaig"
          , "Ardbeg"
          ]
