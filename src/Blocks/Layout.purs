module Ocelot.Block.Layout where

import Prelude

import Halogen.HTML as HH

stickyClasses :: Array HH.ClassName
stickyClasses = HH.ClassName <$>
  [ "fixed"
  , "pin-t"
  , "pin-x"
  , "w-full"
  , "shadow-md"
  , "z-60"
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

mainColClasses :: Array HH.ClassName
mainColClasses = HH.ClassName <$>
  [ "flex-3"
  , "py-16"
  , "px-20"
  ]

sideColClasses :: Array HH.ClassName
sideColClasses = HH.ClassName <$>
  [ "flex-2"
  , "py-16"
  , "px-20"
  ]
