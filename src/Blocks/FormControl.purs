module Ocelot.Block.FormControl (formControl) where

import Prelude

import Ocelot.Block.Type as Type
import Ocelot.Core.Validation (ValidationErrors, htmlE)
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

formControlClasses :: Array HH.ClassName
formControlClasses = HH.ClassName <$>
  [ "w-full" ]

helpTextClasses :: Array HH.ClassName
helpTextClasses = Type.mutedClasses <>
  ( HH.ClassName <$>
    [ "block"
    , "leading-loose"
    ]
  )

errorTextClasses :: Array HH.ClassName
errorTextClasses = HH.ClassName <$>
  [ "leading-loose"
  , "text-red"
  , "font-bold"
  ]

labelClasses :: Array HH.ClassName
labelClasses = HH.ClassName <$>
  [ "block"
  , "font-bold"
  , "leading-loose"
  , "text-black"
  ]

type FormControlProps =
  { helpText :: Maybe String
  , label :: String
  , valid :: Maybe ValidationErrors
  , inputId :: String
  }

formControl
  :: ∀ p i
  . FormControlProps
  -> HH.HTML p i
  -> HH.HTML p i
formControl props html =
  HH.div
    [ HP.classes formControlClasses ]
    [ HH.label
      [ HP.class_ (HH.ClassName "w-full"), HP.for props.inputId ]
      [ label props.label ]
    , HH.div
      [ HP.class_ (HH.ClassName "my-1") ]
      [ html ]
    , helpText props.valid props.helpText
    ]
  where
    helpText (Just errors) _ =
        HH.span
          [ HP.classes errorTextClasses ]
          (HH.fromPlainHTML <$> htmlE errors)
    helpText Nothing (Just x) =
      HH.span
        [ HP.classes helpTextClasses ]
        [ HH.text x ]
    helpText Nothing Nothing = HH.text ""

    label x =
      HH.span
        [ HP.classes labelClasses ]
        [ HH.text x ]
