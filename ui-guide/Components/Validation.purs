module UIGuide.Components.Validation where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (CONSOLE)
import Data.Maybe (Maybe(..))
import Data.Record (modify)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, case_, match)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Ocelot.Block.Card as Card
import Ocelot.Block.FormField as FormField
import Ocelot.Block.Format as Format
import Ocelot.Block.Input as Input
import Ocelot.Data.Record (makeDefaultFormFields, makeDefaultFormInputs, validateSetter, valueSetter)
import Ocelot.Form (Endo, K, Second, check, formFromField, runForm)
import Ocelot.Form.Validation (collapseIfEqual, validateNonEmptyStr, validateStrIsEmail)
import Ocelot.Properties (css)
import Polyform.Validation (V(..), Validation, hoistFnV)
import Type.Prelude (RProxy(..))
import UIGuide.Block.Backdrop as Backdrop
import UIGuide.Block.Documentation as Documentation
import UIGuide.Utilities.Form (EmailError, FormField', FormInput', FormMaybe', PasswordError, PasswordErrorEq)

----------
-- Form

data Query a
  = UpdateContents FieldValueV a
  | ValidateOne FieldValidateV a
  | ValidateAll a

type State =
  { form :: FormInputs
  , raw  :: FormFields
  , result :: Maybe { email :: String, password :: String }
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
              { label: "Email"
              , helpText: Just "Your email will be sold to the highest bidder."
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
              { label: "Password*"
              , helpText: Just "We will store your password in plain text."
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
              { label: "Password (again)*"
              , helpText: Just "These better match!"
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
             pure $ Tuple form value
           Invalid form -> do
             pure $ Tuple form Nothing
      H.modify _ { form = form, result = result }
      pure next


-----
-- Lets' Build A Form

-- Our form is going to consist of these three parts.

-- First, the raw form that the user interacts with in the DOM. These
-- don't have to have string values -- they can be of any type at all.
-- However, all fields will at least have `value` and `shouldValidate`
-- and `shouldValidate` will always be Boolean.
--
-- Note: If we restrict to monoidal values, then we can construct initial
-- forms automatically, without having to write this all out.
signupRawForm :: FormFields
signupRawForm = makeDefaultFormFields (RProxy :: RProxy (FormFieldsT Second))

-- Next, the form we're going to continually run the user's raw input
-- against. It's made up of the same fields as the raw input, but here
-- we have `validated` to represent a validation result, setValue to
-- allow us to set the `value` field of the raw form, and `validate`
-- to set the `shouldValidate` field of the raw form.
--
-- However, every field can be freely extended with more attributes
-- as we'd like. Each field can be extended with different ones -- they
-- don't all have to be the same. Here, I've made sure they all have
-- help text and labels.
--
-- Note: Like the prievious form, this one can be generated with validated
-- set to `Nothing` and setValue/setValidate set to a particular proxy.
--
-- However, the extra attributes can't be generated, so we'll probably write
-- this by hand most of the time.
signupInitialForm :: FormInputs
signupInitialForm = makeDefaultFormInputs (RProxy :: RProxy (FormFieldsT Second))

-- This is where the heavy lifting comes together. We want to be able to compose
-- smaller forms into larger ones. We can extend the two previous records easily
-- by sticking them onto other records. But composing monadic validations and result
-- values is tough. The form below is composed from multiple forms and results
-- in a single record.
signupForm :: ∀ eff. SignupForm (Aff (console :: CONSOLE | eff))
signupForm = { email: _, password: _ }
  <$> emailForm
  <*> passwordForm
  where
    emailForm = formFromField _email $
      hoistFnV validateNonEmptyStr
      >>> hoistFnV (validateStrIsEmail "Not a valid email address.")

    passwordForm = ( { p1: _, p2: _ }
      <$> formFromField _p1 (hoistFnV validateNonEmptyStr)
      <*> formFromField _p2 (hoistFnV validateNonEmptyStr)
      )
      >>> hoistFnV \{ p1, p2 } -> collapseIfEqual p1 p2 _p2


-----
-- Types Involved

-- These are the fields we'll make available in our
-- form. We want an email and two passwords. We can pre-build
-- all sorts of fields with their validations ready to go.
type FormFieldsT f =
  ( email :: f EmailError String
  , p1    :: f PasswordError String
  , p2    :: f PasswordErrorEq String
  )

-- These symbols provide access to the fields in the
-- form record and can be composed with other accessors
_email = SProxy :: SProxy "email"
_p1    = SProxy :: SProxy "p1"
_p2    = SProxy :: SProxy "p2"

-- We can use our form fields record to centralize modifying
-- the record value or validation fields in a single handler
-- in our state
type FieldValueV = Variant (FormFieldsT Second)
type FieldValidateV = Variant (FormFieldsT (K Boolean))

-- This helper function can be used to update the component
-- state for raw fields at the value level. When a user types
-- into an input field or clicks a selection, we'll modify
-- our form in state.
_raw = SProxy :: SProxy "raw"

updateValue :: FieldValueV -> (State -> State)
updateValue = modify _raw <<< valueSetter (RProxy :: RProxy (FormFieldsT Second)) case_

-- This helper function does the same thing, except this time
-- it allows us to modify whether the field should be validated
-- or not in state.
updateValidate :: FieldValidateV -> (State -> State)
updateValidate = modify _raw <<< validateSetter (RProxy :: RProxy (FormFieldsT (K Boolean))) case_

-- We need three types for our form: the raw fields that capture
-- user input; the form inputs that will hold validation results;
-- and our resulting parsed type which will hold the results when
-- we run our form. These three together provide us with a full
-- form we can run.
type FormFields = Record (FormFieldsT FormField')
type FormInputs = Record (FormFieldsT (FormInput' FieldValueV FieldValidateV))

-- Our output data is not the same shape as the input, so we can't just
-- run over FormFieldsT again. It's overkill to make this for just one
-- type but it's here for demonstration purposes
type FormFieldsOutT f =
  ( email    :: f EmailError String
  , password :: f PasswordError String
  )
type FormMaybes = Record (FormFieldsOutT FormMaybe')

-- Finally, we have our actual signup form, made up of those three prior
-- types.
type SignupForm m = Validation m (Endo FormInputs) FormFields FormMaybes

