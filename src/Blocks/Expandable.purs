module Ocelot.Block.Expandable where

import Prelude

import DOM.HTML.Indexed (HTMLdiv, HTMLspan, Interactive)
import Data.Array (snoc)
import Data.Bifunctor (lmap, rmap)
import Data.Foldable (foldr)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.Read (class Read, read)
import Data.Tuple (Tuple(..))
import Halogen.HTML (PropName(..))
import Halogen.HTML as HH
import Halogen.HTML.Core (class IsProp, Prop(..), PropValue)
import Halogen.HTML.Properties as HP
import Halogen.VDom.DOM.Prop (propFromString)
import Ocelot.Block.Icon as Icon
import Ocelot.HTML.Properties ((<&>))
import Unsafe.Coerce (unsafeCoerce)

data Status
  = Collapsed
  | Expanded

instance read :: Read Status where
  read = case _ of
    "collapsed" -> pure Collapsed
    "expanded"  -> pure Expanded
    otherwise   -> Nothing

instance isPropStatus :: IsProp Status where
  toPropValue = propFromString <<< toProp

toProp :: Status -> String
toProp = case _ of
  Collapsed -> "collapsed"
  Expanded  -> "expanded"

toBoolean :: Status -> Boolean
toBoolean Collapsed = false
toBoolean Expanded = true

fromBoolean :: Boolean -> Status
fromBoolean false = Collapsed
fromBoolean true = Expanded

instance heytingAlgebraStatus :: HeytingAlgebra Status where
  ff = Collapsed
  tt = Expanded
  implies a b = not a || b
  conj Expanded Expanded = Expanded
  conj _ _ = Collapsed
  disj Expanded _ = Expanded
  disj _ Expanded = Expanded
  disj _ _ = Collapsed
  not Expanded = Collapsed
  not Collapsed = Expanded

headingClasses :: Array HH.ClassName
headingClasses = HH.ClassName <$>
  [ "flex"
  , "justify-between"
  , "cursor-pointer"
  ]

headingInnerClasses :: Array HH.ClassName
headingInnerClasses = HH.ClassName <$>
  [ "flex-initial"
  ]

chevronClasses :: Array HH.ClassName
chevronClasses = HH.ClassName <$>
  [ "text-grey-70"
  , "text-lg"
  , "leading-loose"
  ]

contentSharedClasses :: Array HH.ClassName
contentSharedClasses = HH.ClassName <$>
  []

contentClasses :: Status -> Array HH.ClassName
contentClasses status_ = contentSharedClasses <>
  ( case status_ of
    Collapsed -> HH.ClassName <$>
      [ "max-h-0"
      , "opacity-0"
      , "overflow-hidden"
      , "transition-1/4-in"
      ]
    Expanded -> HH.ClassName <$>
      [ "max-h-full"
      , "opacity-1"
      , "transition-1/2-out"
      ]
  )

type HTMLexpandable = Interactive ( expanded :: Status )

status :: ∀ r i. Status -> HP.IProp ( expanded :: Status | r ) i
status = HP.prop (PropName "expanded")

-- Takes a row of `IProps` containing the `expanded` label
-- and returns a `Tuple` containing the extracted value as
-- well as the original row, minus the `expanded` label
extractStatus
  :: ∀ r i
   . Array (HH.IProp ( expanded :: Status | r) i)
  -> Tuple Status (Array (HH.IProp r i))
extractStatus =
  foldr f (Tuple Expanded [])
  where
    f (HP.IProp (Property "expanded" expanded)) =
      lmap (const $ coerceExpanded expanded)
    f iprop = rmap $ (flip snoc) $ coerceR iprop

    coerceExpanded :: PropValue -> Status
    coerceExpanded = fromMaybe Expanded <<< read <<< unsafeCoerce

    coerceR :: HH.IProp ( expanded :: Status | r ) i -> HH.IProp r i
    coerceR = unsafeCoerce

heading
  :: ∀ p i
   . Array (HH.IProp HTMLexpandable i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
heading iprops html =
  let (Tuple status_ iprops') = extractStatus iprops in
  HH.header
    ( [ HP.classes headingClasses ] <&> iprops' )
    [ HH.div
      [ HP.classes headingInnerClasses ]
      html
    , HH.div_
      [ chevron_ status_ ]
    ]

chevron
  :: ∀ p i
   . Status
  -> Array (HH.IProp HTMLspan i)
  -> HH.HTML p i
chevron status_ iprops =
  ( case status_ of
    Collapsed -> Icon.expand
    Expanded  -> Icon.collapse
  )
  ( [ HP.classes chevronClasses ] <&> iprops )

chevron_
  :: ∀ p i
   . Status
  -> HH.HTML p i
chevron_ status_ = chevron status_ []

content
  :: ∀ p i
   . Status
  -> Array (HH.IProp HTMLdiv i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
content status_ iprops =
  HH.div
    ( [ HP.classes $ contentClasses status_ ] <&> iprops )

content_
  :: ∀ p i
   . Status
  -> Array (HH.HTML p i)
  -> HH.HTML p i
content_ status_ = content status_ []
