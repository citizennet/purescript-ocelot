module Ocelot.Block.Layout where

import Prelude

import DOM.HTML.Indexed (HTMLdiv)
import Halogen.HTML as HH
import Ocelot.Block.Builder (blockBuilder)

popoverClasses :: Array HH.ClassName
popoverClasses = HH.ClassName <$>
  [ "absolute"
  , "shadow"
  , "z-50"
  , "border"
  , "border-grey-90"
  , "rounded"
  ]

popover ::
  forall p i.
  Array (HH.IProp HTMLdiv i) ->
  Array (HH.HTML p i) ->
  HH.HTML p i
popover = blockBuilder HH.div popoverClasses

popover_ ::
  forall p i.
  Array (HH.HTML p i) ->
  HH.HTML p i
popover_ = popover []

stickyClasses :: Array HH.ClassName
stickyClasses = HH.ClassName <$>
  [ "fixed"
  , "pin-t"
  , "pin-x"
  , "w-full"
  , "shadow-md"
  , "z-60"
  ]

sticky ::
  forall p i.
  Array (HH.IProp HTMLdiv i) ->
  Array (HH.HTML p i) ->
  HH.HTML p i
sticky = blockBuilder HH.div stickyClasses

sticky_ ::
  forall p i.
  Array (HH.HTML p i) ->
  HH.HTML p i
sticky_ = sticky []

containerClasses :: Array HH.ClassName
containerClasses = HH.ClassName <$>
  [ "container"
  , "m-auto"
  ]

container ::
  forall p i.
  Array (HH.IProp HTMLdiv i) ->
  Array (HH.HTML p i) ->
  HH.HTML p i
container = blockBuilder HH.div containerClasses

container_ ::
  forall p i.
  Array (HH.HTML p i) ->
  HH.HTML p i
container_ = container []

sectionClasses :: Array HH.ClassName
sectionClasses = HH.ClassName <$>
  [ "my-8"
  ]

section ::
  forall p i.
  Array (HH.IProp HTMLdiv i) ->
  Array (HH.HTML p i) ->
  HH.HTML p i
section = blockBuilder HH.div sectionClasses

section_ ::
  forall p i.
  Array (HH.HTML p i) ->
  HH.HTML p i
section_ = section []

gridClasses :: Array HH.ClassName
gridClasses = HH.ClassName <$>
  [ "container"
  , "m-auto"
  , "p-8"
  , "flex"
  ]

grid ::
  forall p i.
  Array (HH.IProp HTMLdiv i) ->
  Array (HH.HTML p i) ->
  HH.HTML p i
grid = blockBuilder HH.div gridClasses

grid_ ::
  forall p i.
  Array (HH.HTML p i) ->
  HH.HTML p i
grid_ = grid []

columnClasses :: Array HH.ClassName
columnClasses = HH.ClassName <$>
  [ "flex-1"
  , "p-8"
  ]

column ::
  forall p i.
  Array (HH.IProp HTMLdiv i) ->
  Array (HH.HTML p i) ->
  HH.HTML p i
column = blockBuilder HH.div columnClasses

column_ ::
  forall p i.
  Array (HH.HTML p i) ->
  HH.HTML p i
column_ = column []

mainClasses :: Array HH.ClassName
mainClasses = HH.ClassName <$>
  [ "flex-3"
  , "p-8"
  ]

main ::
  forall p i.
  Array (HH.IProp HTMLdiv i) ->
  Array (HH.HTML p i) ->
  HH.HTML p i
main = blockBuilder HH.div mainClasses

main_ ::
  forall p i.
  Array (HH.HTML p i) ->
  HH.HTML p i
main_ = main []

sideClasses :: Array HH.ClassName
sideClasses = HH.ClassName <$>
  [ "flex-2"
  , "p-8"
  ]

side ::
  forall p i.
  Array (HH.IProp HTMLdiv i) ->
  Array (HH.HTML p i) ->
  HH.HTML p i
side = blockBuilder HH.div sideClasses

side_ ::
  forall p i.
  Array (HH.HTML p i) ->
  HH.HTML p i
side_ = side []
