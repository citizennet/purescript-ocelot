module CN.UI.Block.Type
  ( linkClasses
  , mutedClasses
  ) where

import Prelude

import Halogen.HTML as HH

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
