module Ocelot.Block.Type where

import Prelude

import Halogen.HTML as HH

captionClasses :: Array HH.ClassName
captionClasses = HH.ClassName <$>
  [ "block"
  , "font-light"
  , "my-6"
  , "text-grey-70"
  , "text-sm"
  , "tracking-wide"
  , "uppercase"
  ]

linkClasses :: Array HH.ClassName
linkClasses = HH.ClassName <$>
  [ "text-blue-light"
  , "font-medium"
  , "cursor-pointer"
  ]

mutedClasses :: Array HH.ClassName
mutedClasses = HH.ClassName <$>
  [ "text-grey-dark"
  ]
