module CN.UI.Block.FormControl (formControl) where

import Prelude

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
      [ HP.class_ (HH.ClassName "w-full") ]
      [ label props.label
      , HH.div
          [ HP.class_ (HH.ClassName "my-1") ] 
          [ html ]
      , helpText props.helpText
      ]
    ]
  where
    helpText Nothing = HH.text ""
    helpText (Just x) =
      HH.span
        [ HP.classes helpTextClasses ]
        [ HH.text x ]

    label x =
      HH.span
        [ HP.classes labelClasses ]
        [ HH.text x ]
