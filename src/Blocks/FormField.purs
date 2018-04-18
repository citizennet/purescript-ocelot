module Ocelot.Block.FormField
  ( fieldClasses
  , helpTextClasses
  , errorTextClasses
  , labelClasses
  , field
  , field_
  , fieldSmall
  , fieldSmall_
  , fieldset
  , fieldset_
  ) where

import Prelude

import DOM.HTML.Indexed (HTMLdiv, HTMLp)
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ocelot.Block.Type as Type
import Ocelot.Core.Utils ((<&>))
--  import Ocelot.Core.Validation (ValidationErrors, htmlE)

fieldClasses :: Array HH.ClassName
fieldClasses = HH.ClassName <$>
  [ "w-full"
  , "mb-10"
  ]

helpTextClasses :: Array HH.ClassName
helpTextClasses = Type.mutedClasses <>
  ( HH.ClassName <$>
    [ "block"
    , "pt-3"
    ]
  )

errorTextClasses :: Array HH.ClassName
errorTextClasses = HH.ClassName <$>
  [ "block"
  , "text-red"
  , "font-bold"
  , "pt-3"
  ]

labelClasses :: Array HH.ClassName
labelClasses = HH.ClassName <$>
  [ "block"
  , "font-bold"
  , "leading-loose"
  , "text-black"
  ]

type FieldConfig =
  { helpText :: Maybe String
  , label :: String
  , valid :: Maybe (Array String) -- ValidationErrors
  , inputId :: String
  }

field'
  :: ∀ p i
   . FieldConfig
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
      [ HH.text config.label ]
    , html
    , errorText_ config.valid
    , helpText_ config.helpText
    ]

field
  :: ∀ p i
   . FieldConfig
  -> Array (HH.IProp HTMLdiv i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
field config iprops html =
  field'
    config
    iprops
    ( HH.div [ HP.class_ $ HH.ClassName "my-1" ] html )

field_
  :: ∀ p i
   . FieldConfig
  -> Array (HH.HTML p i)
  -> HH.HTML p i
field_ config = field config []

fieldSmall
  :: ∀ p i
   . FieldConfig
  -> Array (HH.IProp HTMLdiv i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
fieldSmall config iprops html =
  field'
    config
    iprops
    ( HH.div [ HP.class_ $ HH.ClassName "my-1 md:w-1/4" ] html )

fieldSmall_
  :: ∀ p i
   . FieldConfig
  -> Array (HH.HTML p i)
  -> HH.HTML p i
fieldSmall_ config = fieldSmall config []

fieldset
  :: ∀ p i
   . FieldConfig
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
        [ HH.text config.label ]
      , HH.div
        [ HP.class_ (HH.ClassName "my-1") ]
        html
      , errorText_ config.valid
      , helpText_ config.helpText
      ]
    ]

fieldset_
  :: ∀ p i
   . FieldConfig
  -> Array (HH.HTML p i)
  -> HH.HTML p i
fieldset_ config = fieldset config []

errorText
  :: ∀ p i
   . Maybe (Array String) -- ValidationErrors
  -> Array (HH.IProp HTMLp i)
  -> HH.HTML p i
errorText Nothing _ = HH.span_ []
errorText (Just e) iprops =
  HH.p
    ( [ HP.classes errorTextClasses ] <&> iprops )
    []
    --  ( HH.fromPlainHTML <$> htmlE e )

errorText_
  :: ∀ p i
   . Maybe (Array String) -- ValidationErrors
  -> HH.HTML p i
errorText_ v = errorText v []

helpText
  :: ∀ p i
   . Maybe String
  -> Array (HH.IProp HTMLp i)
  -> HH.HTML p i
helpText Nothing _ = HH.span_ []
helpText (Just t) iprops =
  HH.p
    ( [ HP.classes helpTextClasses ] <&> iprops )
    [ HH.text t ]

helpText_
  :: ∀ p i
   . Maybe String
  -> HH.HTML p i
helpText_ t = helpText t []
