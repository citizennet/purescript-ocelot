module UIGuide.Components.Validation where

import Prelude

import Ocelot.Block.Button as Button
import Ocelot.Block.FormControl as FormControl
import Ocelot.Block.Input as Input
import Ocelot.Components.Typeahead as TA
import Ocelot.Core.Typeahead as TACore
import Ocelot.Core.Validation as CV
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff.Timer (TIMER)
import DOM (DOM)
import Data.Array as Array
import Data.Bifunctor as Bifunctor
import Data.Either.Nested (Either4)
import Data.Functor.Coproduct.Nested (Coproduct4)
import Data.Generic.Rep as Generic
import Data.Generic.Rep.Show as Generic.Show
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.StrMap (StrMap, fromFoldable)
import Data.String.Utils as String.Utils
import Data.Tuple (Tuple(..))
import Data.Validation.Semigroup (V, unV)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax (AJAX)
import UIGuide.Block.Component as Component
import UIGuide.Block.Documentation as Documentation
import UIGuide.Utilities.Async as Async

----------
-- Component Types

type State =
  { raw :: UnvalidatedForm
  , errors :: FormErrors
  }


data Query a
  = UpdateTextField Int String a
  | HandleSync (TACore.Message Query TestRecord) a
  | HandleB (TACore.Message Query Async.Todo) a
  | HandleC (TACore.Message Query Async.User) a
  | HandleD (TACore.Message Query Async.User) a
  | Validate FormErrorKey FormVKey a
  | FormSubmit a


----------
-- Child paths

type ChildSlot = Either4 Unit Unit Unit Unit
type ChildQuery eff m = Coproduct4
  (TACore.Query Query TestRecord Void eff m)
  (TACore.Query Query Async.Todo Async.Err eff m)
  (TACore.Query Query Async.User Async.Err eff m)
  (TACore.Query Query Async.User Async.Err eff m)


----------
-- Component definition

-- NOTE: Uses the same effects but does not compose with typeahead effects. Written out again from scratch.
type Effects eff =
  ( avar :: AVAR
  , dom :: DOM
  , ajax :: AJAX
  , timer :: TIMER
  , console :: CONSOLE | eff )

component :: ∀ eff m
  . MonadAff (Effects eff) m
 => H.Component HH.HTML Query Unit Void m
component =
  H.parentComponent
  { initialState: const
      { raw:
        { developers: Nothing
        , todos: []
        , users1: []
        , users2: []
        , email: ""
        , username: "" }
      , errors: Map.empty }
  , render
  , eval
  , receiver: const Nothing
  }
  where
    render
      :: State
      -> H.ParentHTML Query (ChildQuery (Effects eff) m) ChildSlot m
    render st = HH.div_ [ renderForm st ]

    eval
      :: Query
      ~> H.ParentDSL State Query (ChildQuery (Effects eff) m) ChildSlot Void m
    eval (HandleSync message next) = case message of
      TACore.SelectionsChanged _ _ s -> do
         H.modify (_ { raw { developers = TACore.unpackSelection s } } )
         pure next
      _ -> pure next

    eval (HandleB message next) = case message of
      TACore.SelectionsChanged _ _ s -> do
        H.modify (_ { raw { todos = TACore.unpackSelections s }})
        pure next
      _ -> pure next

    eval (HandleC message next) = case message of
      TACore.SelectionsChanged _ _ s -> do
         H.modify (_ { raw { users1 = TACore.unpackSelections s }})
         pure next
      _ -> pure next

    eval (HandleD message next) = case message of
      TACore.SelectionsChanged _ _ s -> do
         H.modify (_ { raw { users2 = TACore.unpackSelections s }})
         _ <- H.query' CP.cp1 unit $ H.action $ TACore.Reset
         pure next
      _ -> pure next

    eval (UpdateTextField i str next) = case i of
      1 -> H.modify (_ { raw { email = str }}) *> pure next
      2 -> H.modify (_ { raw { username = str }}) *> pure next
      _ -> pure next

    eval (Validate eKey vKey next) = do
      st <- H.get
      let v = validateField vKey
      let errors = unV (flip Map.union st.errors) (const $ Map.delete eKey st.errors) v
      H.modify (_ { errors = errors })
      pure next

    eval (FormSubmit next) = do
      st <- H.get
      let validation = runValidation st.raw
      let errors = unV id (const Map.empty) validation
      H.modify (_ { errors = errors })
      pure next


-----
-- Run validation

runValidation :: UnvalidatedForm -> V FormErrors ValidatedForm
runValidation f =
  { developers: _
  , todos: _
  , users1: _
  , users2: _
  , email: _
  , username: _ }
  <$> validateDevelopers f.developers
  <*> validateTodos f.todos
  <*> validateUsers1 f.users1 f.users2
  <*> validateUsers2 f.users2 f.users1
  <*> validateEmail f.email
  <*> validateUsername f.username


-----
-- Top level form types

type UnvalidatedForm =
  { developers :: Maybe TestRecord
  , todos      :: Array Async.Todo
  , users1     :: Array Async.User
  , users2     :: Array Async.User
  , email      :: String
  , username   :: String }

type ValidatedForm =
  { developers :: TestRecord
  , todos      :: ValidatedArray Async.Todo
  , users1     :: ValidatedArray Async.User
  , users2     :: ValidatedArray Async.User
  , email      :: Email
  , username   :: Username }

-----
-- Validation for form fields

validateDevelopers
  :: Maybe TestRecord
  -> V FormErrors TestRecord
validateDevelopers =
  Bifunctor.lmap (Map.singleton FailDevelopers)
  <<< CV.validateNonEmptyMaybe

validateTodos :: Array Async.Todo -> V FormErrors (ValidatedArray Async.Todo)
validateTodos xs =
  Bifunctor.bimap (Map.singleton FailTodos) ValidatedArray
  $ CV.validateNonEmptyArr xs

validateUsers1 :: Array Async.User -> Array Async.User -> V FormErrors (ValidatedArray Async.User)
validateUsers1 users1 users2 =
  Bifunctor.bimap (Map.singleton FailUsers1) ValidatedArray
  $ CV.validateNonEmptyArr users1
  *> CV.validateMinLength 2 "Must select more than one user" users1
  *> validateUserDependence users2 users2

validateUsers2 :: Array Async.User -> Array Async.User -> V FormErrors (ValidatedArray Async.User)
validateUsers2 users2 users1 =
  Bifunctor.bimap (Map.singleton FailUsers2) ValidatedArray
  $ CV.validateNonEmptyArr users2
  *> validateUserDependence users2 users1

validateUserDependence
  :: Array Async.User
  -> Array Async.User
  -> V CV.ValidationErrors (Array Async.User)
validateUserDependence users1 users2 =
  CV.validateDependence
    (\u1 u2 -> Array.length u1 + (Array.length u2) > 4)
    "Users 1 and 2 must combine to 5 or more"
    users1
    users2

validateEmail :: String -> V FormErrors Email
validateEmail email =
  Bifunctor.bimap (Map.singleton FailEmail) Email
  $  CV.validateNonEmptyStr email
  *> CV.validateStrIsEmail email

validateUsername :: String -> V FormErrors Username
validateUsername uname =
  Bifunctor.bimap (Map.singleton FailUsername) (Username <<< String.Utils.fromCharArray)
  $  CV.validateNonEmptyStr uname
  *> CV.validateMinLength 8 "Username must be longer than 8 characters" (String.Utils.toCharArray uname)

-----
-- Specialized types

newtype Email = Email String
newtype Username = Username String
newtype ValidatedArray a = ValidatedArray (Array a)

-----
-- Keys and Map for storing errors on the state

data FormErrorKey
  = FailDevelopers
  | FailTodos
  | FailUsers1
  | FailUsers2
  | FailEmail
  | FailUsername

derive instance eqFormErrorKey :: Eq FormErrorKey
derive instance ordFormErrorKey :: Ord FormErrorKey
derive instance genericFormErrorKey :: Generic.Generic (FormErrorKey) _
instance showFormErrorKey :: Show (FormErrorKey) where
  show = Generic.Show.genericShow

type FormErrors = Map.Map FormErrorKey CV.ValidationErrors

-----
-- Additional function for validating any arbitrary field
-- An extra sum type is required here in order to unify all of the field types
-- So they can get picked up and validated with a single Validate Query handler

data FormVKey
  = DevelopersV (Maybe TestRecord)
  | TodosV (Array Async.Todo)
  | Users1V (Tuple (Array Async.User) (Array Async.User))
  | Users2V (Tuple (Array Async.User) (Array Async.User))
  | EmailV String
  | UsernameV String

validateField :: FormVKey -> V FormErrors Unit
validateField (DevelopersV devs) = const unit <$> validateDevelopers devs
validateField (TodosV todos) = const unit <$> validateTodos todos
validateField (Users1V (Tuple u1 u2)) = const unit <$> validateUsers1 u1 u2
validateField (Users2V (Tuple u2 u1)) = const unit <$> validateUsers2 u2 u1
validateField (EmailV email) = const unit <$> validateEmail email
validateField (UsernameV username) = const unit <$> validateUsername username


----------
-- Rendering

renderForm :: ∀ eff m
  . MonadAff (Effects eff) m
 => State
 -> H.ParentHTML Query (ChildQuery (Effects eff) m) ChildSlot m
renderForm st =
  HH.form
  [ HE.onSubmit $ HE.input_ FormSubmit ]
  [ Documentation.documentation
      { header: "Example Form"
      , subheader: "Test validations and form submission."
      }
      [ Component.component
        { title: "Typeaheads" }
        [ FormControl.formControl
          { label: "Developer"
          , helpText: Just "There are lots of developers to choose from."
          , valid: Map.lookup FailDevelopers st.errors
          , inputId: "devs"
          }
          ( HH.slot' CP.cp1 unit TACore.component
            (TA.defSingle [ HP.placeholder "Search developers...", HP.id_ "devs" ] [] renderItemTestRecord)
            (HE.input HandleSync)
          )
        , FormControl.formControl
          { label: "Todos"
          , helpText: Just "Synchronous todo fetching like you've always wanted."
          , valid: Map.lookup FailTodos st.errors
          , inputId: "todos"
          }
          ( HH.slot' CP.cp2 unit TACore.component
            (TA.defAsyncMulti [ HP.placeholder "Search todos asynchronously...", HP.id_ "todos" ] (\_ -> Async.loadFromSource Async.todos) Async.renderItemTodo)
            (HE.input HandleB)
          )
        , FormControl.formControl
          { label: "Users"
          , helpText: Just "Oh, you REALLY need async, huh."
          , valid: Map.lookup FailUsers1 st.errors
          , inputId: "users1"
          }
          ( HH.slot' CP.cp3 unit TACore.component
            (TA.defAsyncMulti [ HP.placeholder "Search users asynchronously", HP.id_ "users1" ] (\_ -> Async.loadFromSource Async.users) Async.renderItemUser)
            (HE.input HandleC)
          )
        , FormControl.formControl
          { label: "Users 2"
          , helpText: Just "Honestly, this is just lazy."
          , valid: Map.lookup FailUsers2 st.errors
          , inputId: "users2"
          }
          ( HH.slot' CP.cp4 unit TACore.component
            (TA.defAsyncMulti [ HP.placeholder "Search more users...", HP.id_ "users2" ] (\_ -> Async.loadFromSource Async.users) Async.renderItemUser)
            (HE.input HandleD)
          )
        , FormControl.formControl
          { label: "Email"
          , helpText: Just "Dave will spam your email with gang of four patterns"
          , valid: Map.lookup FailEmail st.errors
          , inputId: "email"
          }
          ( Input.input
            [ HP.placeholder "davelovesgangoffour@gmail.com"
            , HP.id_ "email"
            , HE.onBlur (HE.input_ $ Validate FailEmail (EmailV st.raw.email))
            , HE.onValueInput (HE.input $ UpdateTextField 1) ] )
        , FormControl.formControl
          { label: "Username"
          , helpText: Just "Put your name in and we'll spam you forever"
          , valid: Map.lookup FailUsername st.errors
          , inputId: "username"
          }
          ( Input.input
            [ HP.placeholder "Placehold me"
            , HP.id_ "username"
            , HE.onBlur (HE.input_ $ Validate FailUsername (UsernameV st.raw.username))
            , HE.onValueInput (HE.input $ UpdateTextField 2) ] )
        , Button.button
            { type_: Button.Primary }
            [ HP.type_ HP.ButtonSubmit ]
            [ HH.text "Submit" ]
        ]
      ]
  ]


----------
-- Sample data

newtype TestRecord = TestRecord
  { name :: String
  , id :: Int
  }

instance eqTestRecord :: Eq TestRecord where
  eq (TestRecord { id: id'' }) (TestRecord { id: id' }) = id'' == id'

derive instance newtypeTestRecord :: Newtype TestRecord _

renderItemTestRecord :: TA.RenderTypeaheadItem TestRecord
renderItemTestRecord =
  { toStrMap: testToStrMap
  , renderFuzzy: TA.defRenderFuzzy
  , renderItem: (TA.defRenderItem <<< unwrap)
  }

testToStrMap :: TestRecord -> StrMap String
testToStrMap (TestRecord { name, id }) =
  fromFoldable [ Tuple "name" name, Tuple "id" (show id) ]

testRecords :: Array TestRecord
testRecords =
  [ TestRecord { name: "Chris", id: 0 }
  , TestRecord { name: "Dave", id: 1 }
  , TestRecord { name: "Thomas", id: 2 }
  , TestRecord { name: "Forest", id: 3 }
  ]

