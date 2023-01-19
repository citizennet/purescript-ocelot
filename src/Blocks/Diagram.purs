module Ocelot.Block.Diagram where

import Prelude

import DOM.HTML.Indexed (HTMLdiv, HTMLspan)
import Halogen.HTML as Halogen.HTML
import Halogen.HTML.Properties as Halogen.HTML.Properties
import Ocelot.HTML.Properties ((<&>))

percentBarContainerClasses :: Array Halogen.HTML.ClassName
percentBarContainerClasses =
  [ "inline-block"
  , "relative"
  ]
    <#> Halogen.HTML.ClassName

percentBarBackClasses :: Array Halogen.HTML.ClassName
percentBarBackClasses =
  [ "absolute"
  , "bg-grey-80"
  , "h-full"
  , "w-full"
  , "z-10"
  ]
    <#> Halogen.HTML.ClassName

percentBarFrontClasses :: Array Halogen.HTML.ClassName
percentBarFrontClasses =
  [ "absolute"
  , "h-full"
  , "z-20"
  ]
    <#> Halogen.HTML.ClassName

percentBar ::
  forall p i.
  Number ->
  Array (Halogen.HTML.IProp HTMLdiv i) ->
  Array (Halogen.HTML.IProp HTMLspan i) ->
  Halogen.HTML.HTML p i
percentBar percent containerProps frontProps =
  Halogen.HTML.div
    ( [ Halogen.HTML.Properties.classes percentBarContainerClasses ]
        <&> containerProps
    )
    [ Halogen.HTML.span
        [ Halogen.HTML.Properties.classes percentBarBackClasses ]
        []
    , Halogen.HTML.span
        ( [ Halogen.HTML.Properties.attr (Halogen.HTML.AttrName "style") css
          , Halogen.HTML.Properties.classes percentBarFrontClasses
          ]
            <&> frontProps
        )
        []
    ]
  where
  css :: String
  css = "width: " <> show width <> "%"

  width :: Number
  width = clamp 0.0 100.0 percent
