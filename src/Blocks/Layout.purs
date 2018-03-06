module Ocelot.Block.Layout where

import Prelude

import Halogen.HTML as HH

headerClasses :: Array HH.ClassName
headerClasses = HH.ClassName <$>
  [ "fixed"
  , "pin-t"
  , "pin-x"
  , "w-full"
  , "shadow-md"
  ]

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
