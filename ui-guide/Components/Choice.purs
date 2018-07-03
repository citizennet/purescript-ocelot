module UIGuide.Components.Choice where

import Prelude

import Data.Array (mapWithIndex)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Ocelot.Block.Button as Button
import Ocelot.Block.Icon as Icon
import Ocelot.Blocks.Choice as Choice
import Ocelot.HTML.Properties (css)
import Select as Select
import Select.Utils.Setters as SelectSetters
import UIGuide.Block.Backdrop as Backdrop
import UIGuide.Block.Documentation as Documentation

type State = Unit

data Query a
  = HandleSelect (Select.Message Query Platform) a

type Input = Unit

type Message = Void

type ChildSlot = Unit

type ChildQuery = Select.Query Query Platform 

data Platform 
  = Facebook
  | Twitter

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
      HandleSelect message a -> case message of
        Select.Selected x -> do
          _ <- H.query unit ( Select.setVisibility Select.Off )
          pure a

        _ -> do pure a

    render 
      :: State 
      -> H.ParentHTML Query ChildQuery ChildSlot m
    render state =
      HH.div_
        [ Documentation.block_
          { header: "Choice"
          , subheader: "A specialized dropdown for making selections."
          }
          [ Backdrop.backdrop
            [ css "h-40 flex items-center justify-center" ]
            [ HH.slot 
                unit 
                Select.component 
                selectInput 
                ( HE.input HandleSelect )
            ]
          ]
        ]

      where 
        selectInput :: Select.Input Query Platform
        selectInput =
          { debounceTime: Nothing
          , initialSearch: Nothing
          , inputType: Select.Toggle
          , items: [ Facebook, Twitter ] 
          , render: renderPlatformChoice
          }

        renderPlatformChoice state' =
          HH.div
            [ css "flex items-center flex-col" ]
            [ menu
            , Button.buttonPrimary
              ( SelectSetters.setToggleProps [] )
              [ HH.text "Create Campaign Group" ]
            ]
            where
              visibilityClasses = case state'.visibility of
                Select.On -> css ""
                Select.Off -> css "hidden"

              menu =
                Choice.choice
                  ( SelectSetters.setContainerProps [ visibilityClasses ] )
                  [ Choice.header_
                    [ HH.span
                      [ css "font-medium text-grey-50" ]
                      [ HH.text "Advertise on..." ]
                    ]
                  , Choice.body_ $
                      mapWithIndex
                        ( \index item ->
                            Choice.option 
                              ( SelectSetters.setItemProps 
                                  index
                                  [ if Just index == state'.highlightedIndex
                                      then HP.classes Choice.highlightedOptionClasses 
                                      else HP.classes []
                                  ]
                              )
                              ( renderPlatform item )
                        )
                        state'.items
                  ]

              renderPlatform = case _ of
                Facebook ->
                  [ HH.div_ 
                    [ Icon.facebook 
                      [ css "text-fb-blue text-4xl" ] 
                    ]
                  , HH.div_
                    [ HH.p
                      [ css "text-black-20 font-light" ]
                      [ HH.text "Facebook" ]
                    ]
                  ]
                Twitter ->
                  [ HH.div_ 
                    [ Icon.twitter 
                      [ css "text-tw-blue text-4xl" ] ]
                  , HH.div_
                    [ HH.p
                      [ css "text-black-20 font-light" ]
                      [ HH.text "Twitter" ]
                    ]
                  ]

