module UIGuide.Utilities.Effects where

import Control.Monad.Eff.Timer (TIMER)
import Select.Effects (Effects)

type MyEffects eff = ( timer :: TIMER | Effects eff )
