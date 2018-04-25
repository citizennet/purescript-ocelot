module Ocelot.Block.Layout where

import Prelude

import DOM.HTML.Indexed (HTMLdiv)
import Halogen.HTML as HH
import Ocelot.Block.Builder (blockBuilder)

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
  , "p-8"
  ]

columnClasses :: Array HH.ClassName
columnClasses = HH.ClassName <$>
  [ "flex-1"
  , "p-8"
  ]

mainClasses :: Array HH.ClassName
mainClasses = HH.ClassName <$>
  [ "flex-3"
  , "p-8"
  ]

sideClasses :: Array HH.ClassName
sideClasses = HH.ClassName <$>
  [ "flex-2"
  , "p-8"
  ]

sticky
  :: ∀ p i
   . Array (HH.IProp HTMLdiv i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
sticky = blockBuilder HH.div stickyClasses

sticky_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
sticky_ = sticky []

container
  :: ∀ p i
   . Array (HH.IProp HTMLdiv i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
container = blockBuilder HH.div containerClasses

container_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
container_ = container []

column
  :: ∀ p i
   . Array (HH.IProp HTMLdiv i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
column = blockBuilder HH.div columnClasses

column_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
column_ = column []

main
  :: ∀ p i
   . Array (HH.IProp HTMLdiv i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
main = blockBuilder HH.div mainClasses

main_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
main_ = main []

side
  :: ∀ p i
   . Array (HH.IProp HTMLdiv i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
side = blockBuilder HH.div sideClasses

side_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
side_ = side []
