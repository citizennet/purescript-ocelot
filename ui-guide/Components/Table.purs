module UIGuide.Component.Table where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Ratio ((%))
import Halogen as H
import Halogen.HTML as HH
import Ocelot.Block.Button as Button
import Ocelot.Block.Checkbox as Checkbox
import Ocelot.Block.Icon as Icon
import Ocelot.Block.Progress as Progress
import Ocelot.Block.Table as Table
import Ocelot.HTML.Properties (css)
import UIGuide.Block.Backdrop as Backdrop
import UIGuide.Block.Documentation as Documentation

type State = Unit

data Query a = NoOp a

type Input = Unit

type Message = Void

component
  :: ∀ m
   . H.Component HH.HTML Query Input Message m
component =
  H.component
    { initialState: const unit
    , render
    , eval
    , receiver: const Nothing
    }
  where
    eval
      :: Query
      ~> H.ComponentDSL State Query Message m
    eval = case _ of
      NoOp a -> do
        pure a

    render
      :: State
      -> H.ComponentHTML Query
    render _ =
      HH.div_
        [ Documentation.block_
            { header: "Table"
            , subheader: "Tabular Data"
            }
            [ Backdrop.backdrop_
              [ renderTable ]
            ]
        ]
      where
        renderTable =
          Table.table_ $
            [ renderHeader
            ]
            <> renderBody

        renderHeader =
          Table.row_
            [ Table.header  [ css "w-10" ] [ HH.text "" ]
            , Table.header_ [ HH.text "Icon" ]
            , Table.header  [ css "w-2/3 text-left" ] [ HH.text "Description" ]
            , Table.header_ [ HH.text "" ]
            ]

        renderBody =
          Table.row_ <$> ( renderData <$> tableData )

        renderData :: ∀ p i. TestData p i -> Array (HH.HTML p i)
        renderData { name, icon } =
          [ Table.cell_ [ Checkbox.checkbox_ [] [] ]
          , Table.cell  [ css "text-2xl" ] [ icon ]
          , Table.cell  [ css "text-left" ] [ HH.text name ]
          , Table.cell  [ css "text-right" ] [ Button.button_ [ HH.text "Do Nothing" ] ]
          ]

type TestData p i = { name :: String, icon :: HH.HTML p i }

tableData :: ∀ p i. Array (TestData p i)
tableData =
  [ { name: "This is what a back arrow looks like"
    , icon: Icon.back_
    }
  , { name: "This is what a refresh arrow looks like"
    , icon: Icon.refresh_
    }
  , { name: "This is what a settings cog looks like"
    , icon: Icon.settings_
    }
  , { name: "This is what a share button looks like"
    , icon: Icon.share_
    }
  , { name: "This is what an error badge looks like"
    , icon: Icon.error [ css "text-red" ]
    }
  , { name: "This is what a tip bulb looks like"
    , icon: Icon.tip [ css "text-yellow" ]
    }
  , { name: "This is what an info badge looks like"
    , icon: Icon.info [ css "text-blue" ]
    }
  , { name: "This is what a success badge looks like"
    , icon: Icon.success [ css "text-green" ]
    }
  , { name: "This is what the Facebook icon looks like"
    , icon: Icon.facebook [ css "text-fb-blue" ]
    }
  , { name: "This is what the Instagram icon looks like"
    , icon: Icon.instagram [ css "text-ig-brown" ]
    }
  , { name: "This is what the Twitter icon looks like"
    , icon: Icon.twitter [ css "text-tw-blue" ]
    }
  , { name: "This is what a progress bar looks like"
    , icon: Progress.bar (3.0 % 5.0) [ css "w-2/3 h-2" ] [ css "bg-blue h-2" ]
    }
  , { name: "This is what a progress bar with a top caption looks like"
    , icon: HH.div_
        [ HH.p [ css "text-sm pb-2" ] [ HH.text "60% of campaign spent" ]
        , Progress.bar (3.0 % 5.0) [ css "w-2/3 h-2" ] [ css "bg-blue h-2" ]
        ]
    }
  ]
