module Ocelot.Block.FormControl where

import Prelude

import DOM.HTML.Indexed (HTMLdiv, HTMLp)
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ocelot.Block.Type as Type
import Ocelot.Core.Utils ((<&>))
import Ocelot.Core.Validation (ValidationErrors, htmlE)

formControlClasses :: Array HH.ClassName
formControlClasses = HH.ClassName <$>
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

type FormControlConfig =
  { helpText :: Maybe String
  , label :: String
  , valid :: Maybe ValidationErrors
  , inputId :: String
  }

formControl
  :: ∀ p i
   . FormControlConfig
  -> Array (HH.IProp HTMLdiv i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
formControl config iprops html =
  HH.div
    ( [ HP.classes formControlClasses ] <&> iprops )
    [ HH.label
      [ HP.classes labelClasses
      , HP.for config.inputId
      ]
      [ HH.text config.label ]
    , HH.div
      [ HP.class_ (HH.ClassName "my-1") ]
      html
    , errorText_ config.valid
    , helpText_ config.helpText
    ]

formControl_
  :: ∀ p i
   . FormControlConfig
  -> Array (HH.HTML p i)
  -> HH.HTML p i
formControl_ config = formControl config []

fieldset
  :: ∀ p i
   . FormControlConfig
  -> Array (HH.IProp HTMLdiv i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
fieldset config iprops html =
  HH.div
    ( [ HP.classes formControlClasses ] <&> iprops )
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
   . FormControlConfig
  -> Array (HH.HTML p i)
  -> HH.HTML p i
fieldset_ config = fieldset config []

errorText
  :: ∀ p i
   . Maybe ValidationErrors
  -> Array (HH.IProp HTMLp i)
  -> HH.HTML p i
errorText Nothing _ = HH.span_ []
errorText (Just e) iprops =
  HH.p
    ( [ HP.classes errorTextClasses ] <&> iprops )
    ( HH.fromPlainHTML <$> htmlE e )

errorText_
  :: ∀ p i
   . Maybe ValidationErrors
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
