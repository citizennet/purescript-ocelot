module UIGuide.Components.Validation where

import Prelude

import Effect.Aff (Aff)
import Data.Maybe (Maybe(..))
import Record (modify)
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
import Ocelot.Data.Record (makeDefaultFormInputs, validateSetter, inputSetter)
import Ocelot.Form (Form, K, Second, check, formFromField, runForm, FormInput', FormMaybe')
import Ocelot.HTML.Properties (css)
import Polyform.Validation (V(..), hoistFnV)
import Type.Prelude (RProxy(..))
import UIGuide.Block.Backdrop as Backdrop
import UIGuide.Block.Documentation as Documentation
import UIGuide.Utilities.Validation (collapseIfEqual, validateNonEmptyStr, validateStrIsEmail)
import UIGuide.Utilities.Form (EmailError, PasswordError, PasswordErrorEq)

----------
-- Form

data Query a
  = UpdateContents FieldValueV a
  | ValidateOne FieldValidateV a
  | ValidateAll a

type State =
  { form :: FormInputs
  , result :: Maybe { email :: String, password :: String }
  }

component :: H.Component HH.HTML Query Unit Void Aff
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
  initialState = { form: signupInitialForm, result: Nothing }

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
              , error: check st.form.email.result $ match
                  { badEmail: \s -> s
                  , emptyField: \s -> s }
              , inputId: "email"
              }
              [ Input.input
                [ HP.placeholder "address@gmail.com"
                , HP.id_ "email"
                , HE.onBlur $ HE.input_ $ ValidateOne (st.form.email.setValidate true)
                , HE.onValueInput $ HE.input $ UpdateContents <<< st.form.email.setInput
                ]
              ]
            , FormField.field_
              { label: "Password*"
              , helpText: Just "We will store your password in plain text."
              , error: check st.form.p1.result $ match { emptyField: \s -> s }
              , inputId: "password-1-error"
              }
              [ Input.input
                [ HP.placeholder ""
                , HP.id_ "password-1-error"
                , HE.onBlur $ HE.input_ $ ValidateOne (st.form.p1.setValidate true)
                , HE.onValueInput $ HE.input $ UpdateContents <<< st.form.p1.setInput
                ]
              ]
            , FormField.field_
              { label: "Password (again)*"
              , helpText: Just "These better match!"
              , error: check st.form.p2.result $ match
                  { emptyField: \s -> s
                  , notEqual: const "This password does not match the previously-entered password!"
                  }
              , inputId: "password-1-error"
              }
              [ Input.input
                [ HP.placeholder ""
                , HP.id_ "password-1-error"
                , HE.onBlur $ HE.input_ $ ValidateOne (st.form.p2.setValidate true)
                , HE.onValueInput $ HE.input $ UpdateContents <<< st.form.p2.setInput
                ]
              ]
            ]
          ]
        ]
      ]
    ]

  eval :: Query ~> H.ComponentDSL State Query Void Aff
  eval = case _ of
    UpdateContents val next -> do
      H.modify_ $ updateValue val
      pure next

    ValidateOne val next -> do
      H.modify_ $ updateValidate val
      eval $ ValidateAll next

    ValidateAll next -> do
      st <- H.get
      (Tuple form result) <- H.liftAff do
         res <- runForm signupForm st.form
         case res of
           Valid form value -> do
             pure $ Tuple form value
           Invalid form -> do
             pure $ Tuple form Nothing
      H.modify_ _ { form = form, result = result }
      pure next


-----
-- Lets Build A Form

-- We can build forms from FormInput types using formFromField. Then we
-- can compose these into larger forms.
signupForm :: SignupForm Aff
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

-- To create our form inputs, we can use the helper `makeDefaultFormInputs`
-- on a row containing the field names in our form. In the form above, we've
-- used p1, p2, and email, so we'll need those inputs.
signupInitialForm :: FormInputs
signupInitialForm = makeDefaultFormInputs (RProxy :: RProxy (FormFieldsT Second))


-----
-- Form Types Involved

-- Our form needs these three fields, so we'll define them in a row. Our
-- row needs to contain the error type, input type, and output type for each.
-- We'll use `f` here so we can fill in different types and build various
-- type synonyms.
type FormFieldsT f =
  ( email :: f EmailError      String String
  , p1    :: f PasswordError   String String
  , p2    :: f PasswordErrorEq String String
  )

-- This is a bit of boilerplate: we need to define the same names as are in
-- our form all over again here.
_email = SProxy :: SProxy "email"
_p1    = SProxy :: SProxy "p1"
_p2    = SProxy :: SProxy "p2"


-----
-- Variants to Update Form State

-- We'd like to update all our form fields through a single query if possible.
-- To do this, we'll create variants for our `shouldValidate` and `value` fields.
type FieldValueV    = Variant (FormFieldsT Second)
type FieldValidateV = Variant (FormFieldsT (K Boolean))

-- Next, we'll create the functions that will actually update the relevant fields.
-- These two helper functions will allow us to update the value and shouldValidate
-- fields respectively.
_form = SProxy :: SProxy "form"

updateValue :: FieldValueV -> (State -> State)
updateValue = modify _form <<< inputSetter (RProxy :: RProxy (FormFieldsT Second)) case_

updateValidate :: FieldValidateV -> (State -> State)
updateValidate = modify _form <<< validateSetter (RProxy :: RProxy (FormFieldsT (K Boolean))) case_

-- We can finally write the type for our form in state, now that we have our variants:
type FormInputs = Record (FormFieldsT (FormInput' FieldValueV FieldValidateV))

-- We'll need one more thing: our output type. This form parses to a different output
-- than its input: (p1, p2, email) vs. (password, email).
type FormFieldsOutT f =
  ( email    :: f EmailError    String String
  , password :: f PasswordError String String
  )
type FormOutputs = Record (FormFieldsOutT FormMaybe')

-- Finally, we can create our overall form type!
type SignupForm m = Form m FormInputs FormOutputs
