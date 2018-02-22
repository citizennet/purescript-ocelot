module CN.UI.Block.FormControl (formControl) where

import Prelude

import CN.UI.Core.Validation (ValidationErrors, htmlE)
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

formControlClasses :: Array HH.ClassName
formControlClasses = HH.ClassName <$>
  [ "w-full" ]

helpTextClasses :: Array HH.ClassName
helpTextClasses = HH.ClassName <$>
  [ "block"
  , "leading-loose"
  , "text-grey-darker"
  , "text-sm"
  ]

errorTextClasses :: Array HH.ClassName
errorTextClasses = HH.ClassName <$>
  [ "leading-loose"
  , "text-red"
  , "text-sm"
  , "font-bold"
  ]

labelClasses :: Array HH.ClassName
labelClasses = HH.ClassName <$>
  [ "block"
  , "font-bold"
  , "leading-loose"
  , "text-black"
  , "text-sm"
  ]

type FormControlProps =
  { helpText :: Maybe String
  , label :: String
  , valid :: Maybe ValidationErrors
  }

formControl
  :: ∀ p i
  . FormControlProps
  -> HH.HTML p i
  -> HH.HTML p i
formControl props html =
  HH.div
    [ HP.classes formControlClasses ]
    [ HH.span
      [ HP.class_ (HH.ClassName "w-full") ]
      [ label props.label
      , HH.div
          [ HP.class_ (HH.ClassName "my-1") ]
          [ html ]
      , helpText props.valid props.helpText
      ]
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
