module Ocelot.Block.FormControl
  ( formControl
  , fieldset
  , helpText
  ) where

import Prelude

import Ocelot.Block.Type as Type
import Ocelot.Core.Validation (ValidationErrors, htmlE)
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

formControlClasses :: Array HH.ClassName
formControlClasses = HH.ClassName <$>
  [ "w-full"
  , "mb-10"
  ]

helpTextClasses :: Array HH.ClassName
helpTextClasses = Type.mutedClasses <>
  ( HH.ClassName <$> [ "block", "pt-2" ] )

errorTextClasses :: Array HH.ClassName
errorTextClasses = HH.ClassName <$>
  [ "block"
  , "text-red"
  , "font-bold"
  , "pt-2"
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

fieldset
  :: ∀ p i
   . FormControlProps
  -> HH.HTML p i
  -> HH.HTML p i
fieldset props html =
  HH.div
    [ HP.classes formControlClasses ]
    [ HH.fieldset
      []
      [ HH.legend
        [ HP.class_ (HH.ClassName "w-full") ]
        [ label props.label ]
      , HH.div
        [ HP.class_ (HH.ClassName "my-1") ]
        [ html ]
      , helpText props.valid props.helpText
      ]
    ]

label :: ∀ p i. String -> HH.HTML p i
label x =
  HH.span
    [ HP.classes labelClasses ]
    [ HH.text x ]

helpText
  :: ∀ p i
   . Maybe ValidationErrors
  -> Maybe String
  -> HH.HTML p i
helpText errors help =
  HH.div_
  [ case errors of
      Just e  -> HH.span
        [ HP.classes errorTextClasses ]
        ( HH.fromPlainHTML <$> htmlE e )
      Nothing -> HH.text ""
  , case help of
      Just t -> HH.span
        [ HP.classes helpTextClasses ]
        [ HH.text t ]
      Nothing -> HH.text ""
  ]
