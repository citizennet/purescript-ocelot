module UIGuide.Components.Validation where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Aff.Console (log) as Console
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, inj, match)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Ocelot.Block.Card as Card
import Ocelot.Block.FormField as FormField
import Ocelot.Block.Format as Format
import Ocelot.Block.Input as Input
import Ocelot.Core.Form (Endo, FormField, FormInput, Id, K, formFromField, runForm, setValidate, setValue, check)
import Ocelot.Core.Utils (css)
import Ocelot.Core.Validation (collapseIfEqual, validateNonEmptyStr, validateStrIsEmail)
import Polyform.Validation (V(..), Validation, hoistFnV)
import UIGuide.Block.Backdrop as Backdrop
import UIGuide.Block.Documentation as Documentation

----------
-- Form

data Query a
  = UpdateContents FieldValueV a
  | ValidateOne FieldValidateV a
  | ValidateAll a

type State =
  { form :: FormInputs
  , raw  :: FormFields
  , result :: Maybe User
  }

type User =
  { email :: String
  , password :: String
  }

component :: ∀ eff. H.Component HH.HTML Query Unit Void (Aff (console :: CONSOLE | eff))
component =
  H.lifecycleComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just $ H.action ValidateAll
    , finalizer: Nothing
    }
  where
  initialState :: State
  initialState = { raw: signupRawForm, form: signupInitialForm, result: Nothing }

  signupRawForm :: FormFields
  signupRawForm =
    { email: { value: "", shouldValidate: false }
    , p1:    { value: "", shouldValidate: false }
    , p2:    { value: "", shouldValidate: false }
    }

  signupInitialForm :: FormInputs
  signupInitialForm =
    { email: { validated: Nothing, setValue: inj _email, setValidate: inj _email }
    , p1:    { validated: Nothing, setValue: inj _p1, setValidate: inj _p1 }
    , p2:    { validated: Nothing, setValue: inj _p2, setValidate: inj _p2 }
    }

  signupForm :: SignupForm (Aff (console :: CONSOLE | eff))
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

  render :: State -> H.ComponentHTML Query
  render st =
    HH.div_
    [ Documentation.block_
      { header: "Text Field Form"
      , subheader: "Validates composed form fields."
      }
      [ Backdrop.backdrop_
        [ Backdrop.content [ css "flex" ]
          [ Card.card
            [ HP.class_ $ HH.ClassName "flex-1" ]
            [ HH.h3
              [ HP.classes Format.captionClasses ]
              [ HH.text "Fields" ]
            , FormField.field_
              { label: "Email*"
              , helpText: Just "Provide a valid email address."
              , error: check st.form.email.validated $ match
                  { badEmail: \s -> s
                  , emptyField: \s -> s }
              , inputId: "email"
              }
              [ Input.input
                [ HP.placeholder "address@gmail.com"
                , HP.id_ "email"
                , HE.onBlur $ HE.input_ $ ValidateOne (st.form.email.setValidate true)
                , HE.onValueInput $ HE.input $ UpdateContents <<< st.form.email.setValue
                ]
              ]
            , FormField.field_
              { label: "Password 1*"
              , helpText: Just "Write your password."
              , error: check st.form.p1.validated $ match { emptyField: \s -> s }
              , inputId: "password-1-error"
              }
              [ Input.input
                [ HP.placeholder ""
                , HP.id_ "password-1-error"
                , HE.onBlur $ HE.input_ $ ValidateOne (st.form.p1.setValidate true)
                , HE.onValueInput $ HE.input $ UpdateContents <<< st.form.p1.setValue
                ]
              ]
            , FormField.field_
              { label: "Password 2*"
              , helpText: Just "Write your password again for confirmation."
              , error: check st.form.p2.validated $ match
                  { emptyField: \s -> s
                  , notEqual: const "This password does not match the previously-entered password!"
                  }
              , inputId: "password-1-error"
              }
              [ Input.input
                [ HP.placeholder ""
                , HP.id_ "password-1-error"
                , HE.onBlur $ HE.input_ $ ValidateOne (st.form.p2.setValidate true)
                , HE.onValueInput $ HE.input $ UpdateContents <<< st.form.p2.setValue
                ]
              ]
            ]
          ]
        ]
      ]
    ]

  eval :: Query ~> H.ComponentDSL State Query Void (Aff ( console :: CONSOLE | eff))
  eval = case _ of
    UpdateContents val next -> do
      H.modify $ updateValue val
      pure next

    ValidateOne val next -> do
      H.modify $ updateValidate val
      eval $ ValidateAll next

    ValidateAll next -> do
      st <- H.get
      (Tuple form result) <- H.liftAff do
         res <- runForm signupForm st.form st.raw
         case res of
           Valid form value -> do
             case value of
               Just v -> do
                 Console.log "Successful parse!"
                 Console.log $ "Email: " <> v.email <> "\nPassword: " <> v.password
               Nothing -> do
                 Console.log "Passed validation, but didn't parse."
             pure $ Tuple form value
           Invalid form -> do
             Console.log "Failed validation."
             pure $ Tuple form Nothing

      H.modify _ { form = form, result = result }
      pure next



-----
-- Form Helpers

updateValue :: FieldValueV -> (State -> State)
updateValue = match
  { p1:    setValue _p1
  , p2:    setValue _p2
  , email: setValue _email
  }

updateValidate :: FieldValidateV -> (State -> State)
updateValidate = match
  { p1:    setValidate _p1
  , p2:    setValidate _p2
  , email: setValidate _email
  }


-----
-- Form Types

-- Fields available in this form's input
type FormFieldsT f =
  ( email :: f String
  , p1    :: f String
  , p2    :: f String
  )

-- Proxies for field labels for convenience (allows us to
-- use just one query to access all labels in the record).
_email = SProxy :: SProxy "email"
_p1 = SProxy :: SProxy "p1"
_p2 = SProxy :: SProxy "p2"

-- The error types used on our fields (usually this would be kept with the field definitions
-- and not with the form. That makes these portable.
type EmailError = Array (Variant (emptyField :: String, badEmail :: String))
type Password1Error = Array (Variant (emptyField :: String))
type Password2Error = Array (Variant (emptyField :: String, notEqual :: Tuple (Maybe String) (Maybe String)))

-- The variants used to access each field in the form
type FieldValueV = Variant (FormFieldsT Id)
type FieldValidateV = Variant (FormFieldsT (K Boolean))

-- The form types we need: the raw form, the initial form, and the overall signup form.
type FormFields = Record (FormFieldsT FormField)

type FormInputs =
  { email :: FormInput () (String -> FieldValueV) (Boolean -> FieldValidateV) EmailError String
  , p1 :: FormInput () (String -> FieldValueV) (Boolean -> FieldValidateV) Password1Error String
  , p2 :: FormInput () (String -> FieldValueV) (Boolean -> FieldValidateV) Password2Error String
  }

type SignupForm m = Validation m (Endo FormInputs) FormFields { email :: Maybe String, password :: Maybe String }

