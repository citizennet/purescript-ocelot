module Ocelot.Block.FormField where

import Prelude

import DOM.HTML.Indexed (HTMLdiv)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ocelot.Block.Format as Format
import Ocelot.HTML.Properties (css, (<&>))

fieldClasses :: Array HH.ClassName
fieldClasses = HH.ClassName <$>
  [ "w-full"
  , "mb-10"
  ]

helpTextClasses :: Array HH.ClassName
helpTextClasses = Format.mutedClasses <>
  ( HH.ClassName <$>
    [ "block"
    , "font-light"
    , "pt-3"
    ]
  )

errorTextClasses :: Array HH.ClassName
errorTextClasses = HH.ClassName <$>
  [ "block"
  , "text-red"
  , "font-medium"
  , "pt-3"
  ]

labelClasses :: Array HH.ClassName
labelClasses = HH.ClassName <$>
  [ "block"
  , "font-medium"
  , "leading-loose"
  , "text-black-20"
  ]

type FieldConfig p i =
  { label :: HH.PlainHTML
  , inputId :: String
  , helpText :: Array (HH.HTML p i)
  , error :: Array (HH.HTML p i)
  }

field'
  :: ∀ p i
   . FieldConfig p i
  -> Array (HH.IProp HTMLdiv i)
  -> HH.HTML p i
  -> HH.HTML p i
field' config iprops html =
  HH.div
    ( [ HP.classes fieldClasses ] <&> iprops )
    [ HH.label
      [ HP.classes labelClasses
      , HP.for config.inputId
      ]
      [ HH.fromPlainHTML config.label ]
    , html
    , error_ config.error
    , helpText_ config.helpText
    ]

field
  :: ∀ p i
   . FieldConfig p i
  -> Array (HH.IProp HTMLdiv i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
field config iprops html =
  field'
    config
    iprops
    ( HH.div [ css "my-1" ] html )

field_
  :: ∀ p i
   . FieldConfig p i
  -> Array (HH.HTML p i)
  -> HH.HTML p i
field_ config = field config []

fieldSmall
  :: ∀ p i
   . FieldConfig p i
  -> Array (HH.IProp HTMLdiv i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
fieldSmall config iprops html =
  field'
    config
    iprops
    ( HH.div [ css "my-1 md:w-1/4" ] html )

fieldSmall_
  :: ∀ p i
   . FieldConfig p i
  -> Array (HH.HTML p i)
  -> HH.HTML p i
fieldSmall_ config = fieldSmall config []

fieldMid
  :: ∀ p i
   . FieldConfig p i
  -> Array (HH.IProp HTMLdiv i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
fieldMid config iprops html =
  field'
    config
    iprops
    ( HH.div [ css "my-1 md:w-1/2" ] html )

fieldMid_
  :: ∀ p i
   . FieldConfig p i
  -> Array (HH.HTML p i)
  -> HH.HTML p i
fieldMid_ config = fieldMid config []

fieldset
  :: ∀ p i
   . FieldConfig p i
  -> Array (HH.IProp HTMLdiv i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
fieldset config iprops html =
  HH.div
    ( [ HP.classes fieldClasses ] <&> iprops )
    [ HH.fieldset
      []
      [ HH.legend
        [ HP.classes labelClasses ]
        [ HH.fromPlainHTML config.label ]
      , HH.div
        [ css "my-1" ]
        html
      , error_ config.error
      , helpText_ config.helpText
      ]
    ]

fieldset_
  :: ∀ p i
   . FieldConfig p i
  -> Array (HH.HTML p i)
  -> HH.HTML p i
fieldset_ config = fieldset config []

error
  :: ∀ p i
   . Array (HH.IProp HTMLdiv i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
error iprops = HH.div $ [ HP.classes errorTextClasses ] <&> iprops

error_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
error_ = error []

helpText
  :: ∀ p i
   . Array (HH.IProp HTMLdiv i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
helpText iprops = HH.div $ [ HP.classes helpTextClasses ] <&> iprops

helpText_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
helpText_ = helpText []
