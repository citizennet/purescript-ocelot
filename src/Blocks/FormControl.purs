module CN.UI.Block.FormControl (formControl) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Validation.Semigroup (V, isValid, unV)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

formControlClasses :: Array HH.ClassName
formControlClasses = HH.ClassName <$>
  [ "w-full" ]

helpTextClasses :: Array HH.ClassName
helpTextClasses = HH.ClassName <$>
  [ "leading-loose"
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
  [ "font-bold"
  , "leading-loose"
  , "text-black"
  , "text-sm"
  ]

type FormControlProps err item =
  { helpText :: Maybe String
  , label :: String
  , valid :: Maybe (V err item)
  }

formControl
  :: ∀ p i err item
   . Show err
  => FormControlProps err item
  -> HH.HTML p i
  -> HH.HTML p i
formControl props html =
  HH.div
    [ HP.classes formControlClasses ]
    [ HH.label
      [ HP.class_ (HH.ClassName "w-full") ]
      [ label props.label
      , html
      , helpText props.valid props.helpText
      ]
    ]
  where
    helpText (Just v) text
      | isValid v = helpText Nothing text
      | otherwise =
        HH.span
          [ HP.classes errorTextClasses ]
          [ HH.text $ unV show (const "") v ]
    helpText Nothing (Just x) =
      HH.span
        [ HP.classes helpTextClasses ]
        [ HH.text x ]
    helpText Nothing Nothing = HH.text ""

    label x =
      HH.span
        [ HP.classes labelClasses ]
        [ HH.text x ]
