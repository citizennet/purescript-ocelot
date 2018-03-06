module Ocelot.Block.FormLayout where

import Prelude

import Halogen.HTML as HH

containerClasses :: Array HH.ClassName
containerClasses = HH.ClassName <$>
  [ "container"
  , "m-auto"
  , "flex"
  ]

columnClasses :: Array HH.ClassName
columnClasses = HH.ClassName <$>
  [ "flex-1"
  , "py-16"
  , "px-20"
  ]
