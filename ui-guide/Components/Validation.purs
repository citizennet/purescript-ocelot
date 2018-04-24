module UIGuide.Components.Validation where

import Prelude

import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff.Random (RANDOM)
import Data.Either (Either)
import Data.Maybe (Maybe(Nothing))
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple)
import Data.Variant (Variant, inj)
import Halogen as H
import Halogen.HTML as HH
import Ocelot.Core.Form (Endo, FormField, Id, K, formFromField)
import Ocelot.Core.Validation (collapseIfEqual, validateNonEmptyStr, validateStrIsEmail)
import Polyform.Validation (Validation, hoistFnV)


----------
-- Form

data Query a
  = NoOp a
  --  = UpdateContents FormFieldValue a
  --  | ValidateOne FormFieldValidate a
  --  | ValidateAll a

type State = Unit
  --  { form    :: _
  --  , raw     :: _
  --  }

component :: ∀ eff m
  . MonadAff ( console :: CONSOLE, random :: RANDOM | eff ) m
 => H.Component HH.HTML Query Unit Void m
component =
  H.lifecycleComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    , initializer: Nothing -- Just $ H.action ValidateAll
    , finalizer: Nothing
    }
  where
  initialState :: State
  initialState = unit --  { raw: signupRawForm, form: signupInitialForm }

  render :: State -> H.ComponentHTML Query
  render st = HH.div_ []

  eval :: Query ~> H.ComponentDSL State Query Void m
  eval = case _ of
    NoOp a -> pure a
    --  UpdateContents val next -> do
    --    H.modify $ updateValue val
    --    pure next
    --
    --  ValidateOne val next -> do
    --    H.modify $ updateValidate val
    --    eval $ ValidateAll next
    --
    --  ValidateAll next -> do
    --    st <- H.get
    --    pure next


_email = SProxy :: SProxy "email"
_p1 = SProxy :: SProxy "p1"
_p2 = SProxy :: SProxy "p2"

-- - attrs: Arbitrary additional labels we want available for
--          input, like help text
-- - vl: A variant pointing to the value field in the accompanying
--       raw form for our form representation.
-- - vd: A variant pointing to the validation field in the accompanying
--       raw form for our form representation
-- - e: Our error type for our input value if it fails validation
-- - a: Our successfully parsed type if it passes validation
--
-- Note: Value wrapped in Maybe to represent a case where we haven't
-- validated the field yet.
--  type FormInput attrs vl vd e a =
--    { validated   :: Maybe (Either e a)
--    , setValue    :: vl
--    , setValidate :: vd
--    | attrs
--    }

type FormFieldsT f =
  ( email :: f String
  , p1    :: f String
  , p2    :: f String
  )

-- We can access the 'value' part of each input like this:
type FieldValueV = Variant (FormFieldsT Id)

-- and the 'validation' part like this:
type FieldValidateV = Variant (FormFieldsT (K Boolean))

-- Now we can easily create our raw form type...
type RawForm = Record (FormFieldsT FormField)

signupRawForm :: RawForm
signupRawForm =
  { email: { value: "", shouldValidate: false }
  , p1:    { value: "", shouldValidate: false }
  , p2:    { value: "", shouldValidate: false }
  }


-- Making this one ain't so easy...haven't figured it out.

signupInitialForm :: forall t141 t143 t144 t147 t148 t150 t152 t153 t156 t157 t159 t161 t162 t165 t166.
   { email :: { validated :: Maybe t141
              , shouldValidate :: Boolean
              , updateValue :: t144 -> Variant ( email :: t144 | t143 )
              , updateValidate :: t148 -> Variant ( email :: t148 | t147 )
              }
   , p1 :: { validated :: Maybe t150
           , shouldValidate :: Boolean
           , updateValue :: t153 -> Variant ( p1 :: t153 | t152 )
           , updateValidate :: t157 -> Variant ( p1 :: t157 | t156 )
           }
   , p2 :: { validated :: Maybe t159
           , shouldValidate :: Boolean
           , updateValue :: t162 -> Variant ( p2 :: t162 | t161 )
           , updateValidate :: t166 -> Variant ( p2 :: t166 | t165 )
           }
   }
signupInitialForm =
  { email: { validated: Nothing, shouldValidate: false, updateValue: inj _email, updateValidate: inj _email }
  , p1:    { validated: Nothing, shouldValidate: false, updateValue: inj _p1, updateValidate: inj _p1 }
  , p2:    { validated: Nothing, shouldValidate: false, updateValue: inj _p2, updateValidate: inj _p2 }
  }


-- Good God above, this one's even worse! It'd take all fuckin' year to write this type signature

signupForm :: forall t102 t103 t104 t105 t126 t136 t137 t138 t37 t38 t39 t50 t54 t55 t56.
   Monad t105 => Validation t105
                   (Endo
                      { p1 :: { validated :: Maybe
                                               (Either
                                                  (Array
                                                     (Variant
                                                        ( emptyField :: String
                                                        | t50
                                                        )
                                                     )
                                                  )
                                                  String
                                               )
                              , setValue :: t38
                              , setValidate :: t37
                              | t39
                              }
                      , p2 :: { validated :: Maybe
                                               (Either
                                                  (Array
                                                     (Variant
                                                        ( emptyField :: String
                                                        , notEqual :: Tuple (Maybe String) (Maybe String)
                                                        | t136
                                                        )
                                                     )
                                                  )
                                                  String
                                               )
                              , setValue :: t55
                              , setValidate :: t54
                              | t56
                              }
                      , email :: { validated :: Maybe
                                                  (Either
                                                     (Array
                                                        (Variant
                                                           ( emptyField :: String
                                                           , badEmail :: String
                                                           | t126
                                                           )
                                                        )
                                                     )
                                                     String
                                                  )
                                 , setValue :: t103
                                 , setValidate :: t102
                                 | t104
                                 }
                      | t138
                      }
                   )
                   { p1 :: { value :: String
                           , shouldValidate :: Boolean
                           }
                   , p2 :: { value :: String
                           , shouldValidate :: Boolean
                           }
                   , email :: { value :: String
                              , shouldValidate :: Boolean
                              }
                   | t137
                   }
                   { email :: Maybe String
                   , password :: Maybe String
                   }
signupForm = { email: _, password: _ }
  <$> emailForm
  <*> passwordForm
  where
    emailForm = formFromField (SProxy :: SProxy "email") $
      hoistFnV validateNonEmptyStr
      >>> hoistFnV (validateStrIsEmail "Not a valid email address.")

    passwordForm = ( { p1: _, p2: _ }
      <$> formFromField _p1 (hoistFnV validateNonEmptyStr)
      <*> formFromField _p2 (hoistFnV validateNonEmptyStr)
      )
      >>> hoistFnV \{ p1, p2 } -> collapseIfEqual p1 p2 _p1 _p2

