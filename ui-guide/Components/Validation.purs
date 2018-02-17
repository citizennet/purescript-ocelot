module UIGuide.Components.Validation where

import Prelude

import CN.UI.Block.Button as Button
import CN.UI.Block.FormControl as FormControl
import CN.UI.Block.Input as Input
import CN.UI.Components.Typeahead as TAInput
import CN.UI.Core.Typeahead as TA
import CN.UI.Core.Validation (validateNonEmptyStr, validateNonEmptyArr, validateStrIsEmail, validateMinLength, validateDependence, ValidationError, ValidationErrors)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff.Timer (TIMER)
import DOM (DOM)
import Data.Array as Array
import Data.Bifunctor (rmap)
import Data.Bifunctor as Bifunctor
import Data.Const (Const(..))
import Data.Either (fromRight)
import Data.Either.Nested (Either4)
import Data.Foldable (class Foldable)
import Data.Foldable as Foldable
import Data.Functor.Coproduct.Nested (Coproduct4)
import Data.Generic.Rep as Generic
import Data.Generic.Rep.Eq as Generic.Eq
import Data.Generic.Rep.Show as Generic.Show
import Data.Int (fromString)
import Data.List.NonEmpty (NonEmptyList(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (class Monoid)
import Data.Newtype (class Newtype, unwrap)
import Data.StrMap (StrMap, fromFoldable, insert)
import Data.String as String
import Data.String.Regex as Regex
import Data.String.Regex.Flags as Regex.Flags
import Data.String.Utils as String.Utils
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..))
import Data.Validation.Semigroup (V, invalid, unV)
import Data.Variant (Variant, SProxy(..), case_, on, inj)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax (AJAX)
import Partial.Unsafe (unsafePartial)
import Type.Proxy (Proxy(..))
import Type.Row (class RowToList, Cons, Nil, RLProxy(..), RProxy(..), kind RowList)
import UIGuide.Block.Component as Component
import UIGuide.Block.Documentation as Documentation
import UIGuide.Utilities.Async as Async

----------
-- Component Types

type State =
  { raw :: UnvalidatedForm
  , validation :: FormErrors
  }


data Query a
  = UpdateTextField Int String a
  | HandleA (TA.TypeaheadMessage Query TestRecord Void Void) a
  | HandleB (TA.TypeaheadMessage Query Async.Todo (Async.Source Async.Todo) Async.Err) a
  | HandleC (TA.TypeaheadMessage Query Async.User (Async.Source Async.User) Async.Err) a
  | HandleD (TA.TypeaheadMessage Query Async.User (Async.Source Async.User) Async.Err) a
  | FormSubmit a


----------
-- Child paths

type ChildSlot = Either4 Unit Unit Unit Unit
type ChildQuery eff m = Coproduct4
  (TA.TypeaheadQuery Query TestRecord Void Void eff m)
  (TA.TypeaheadQuery Query Async.Todo (Async.Source Async.Todo) Async.Err eff m)
  (TA.TypeaheadQuery Query Async.User (Async.Source Async.User) Async.Err eff m)
  (TA.TypeaheadQuery Query Async.User (Async.Source Async.User) Async.Err eff m)


cp1 :: ∀ eff m. CP.ChildPath (TA.TypeaheadQuery Query TestRecord Void Void eff m) (ChildQuery eff m) Unit ChildSlot
cp1 = CP.cp1

cp2 :: ∀ eff m. CP.ChildPath (TA.TypeaheadQuery Query Async.Todo (Async.Source Async.Todo) Async.Err eff m) (ChildQuery eff m) Unit ChildSlot
cp2 = CP.cp2

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
        { developers: []
        , todos: []
        , users1: []
        , users2: []
        , email: ""
        , username: "" }
      , validation: Map.empty }
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
    eval (HandleA message next) = case message of
      _ -> pure next

    eval (HandleB message next) = case message of
      TA.RequestData syncMethod -> load CP.cp2 syncMethod *> pure next
      _ -> pure next

    eval (HandleC message next) = case message of
      TA.RequestData syncMethod -> load CP.cp3 syncMethod *> pure next
      _ -> pure next

    eval (HandleD message next) = case message of
      TA.RequestData syncMethod -> load CP.cp4 syncMethod *> pure next
      _ -> pure next

    eval (UpdateTextField i str next) = case i of
      1 -> H.modify (_ { raw { email = str }}) *> pure next
      2 -> H.modify (_ { raw { username = str }}) *> pure next
      _ -> pure next

    eval (FormSubmit next) = do
      -- Collect data from components
      devs   <- H.query' CP.cp1 unit (H.request TA.Selections)
      todos  <- H.query' CP.cp2 unit (H.request TA.Selections)
      users1 <- H.query' CP.cp3 unit (H.request TA.Selections)
      users2 <- H.query' CP.cp4 unit (H.request TA.Selections)

      H.modify
        (_ { raw
              { developers = fromMaybe [] $ TA.unpackSelections <$> devs
              , todos = fromMaybe [] $ TA.unpackSelections <$> todos
              , users1 = fromMaybe [] $ TA.unpackSelections <$> users1
              , users2 = fromMaybe [] $ TA.unpackSelections <$> users2 }
           })

      -- Validate data
      st <- H.get
      let validation = runValidation st.raw
      let formErrors = unV id (const Map.empty) validation
      H.modify (_ { validation = formErrors })
      pure next


    load :: ∀ item
      . CP.ChildPath (TA.TypeaheadQuery Query item (Async.Source item) Async.Err (Effects eff) m) (ChildQuery (Effects eff) m) Unit ChildSlot
     -> TA.SyncMethod (Async.Source item) Async.Err (Array item)
     -> H.ParentDSL State Query (ChildQuery (Effects eff) m) ChildSlot Void m Unit
    load cp m = do
      res <- H.liftAff $ Async.load m
      case TA.maybeReplaceItems res m of
        Nothing -> pure unit
        (Just newSync) -> do
           _ <- H.query' cp unit $ H.action $ TA.FulfillRequest newSync
           pure unit


----------
-- Validation

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
  { developers :: Array TestRecord
  , todos      :: Array Async.Todo
  , users1     :: Array Async.User
  , users2     :: Array Async.User
  , email      :: String
  , username   :: String }

type ValidatedForm =
  { developers :: ValidatedArray TestRecord
  , todos      :: ValidatedArray Async.Todo
  , users1     :: ValidatedArray Async.User
  , users2     :: ValidatedArray Async.User
  , email      :: Email
  , username   :: Username }

-----
-- Validation for form fields

validateDevelopers :: Array TestRecord -> V FormErrors (ValidatedArray TestRecord)
validateDevelopers xs =
  Bifunctor.bimap (toMap FailDevelopers) ValidatedArray
  $ validateMinLength 2 "Must select more than one developer" xs

validateTodos :: Array Async.Todo -> V FormErrors (ValidatedArray Async.Todo)
validateTodos xs =
  Bifunctor.bimap (toMap FailTodos) ValidatedArray
  $ validateNonEmptyArr "Todos cannot be empty" xs

validateUsers1 :: Array Async.User -> Array Async.User -> V FormErrors (ValidatedArray Async.User)
validateUsers1 users1 users2 =
  Bifunctor.bimap (toMap FailUsers1) ValidatedArray
  $ validateNonEmptyArr "Users cannot be empty" users1
  *> validateMinLength 2 "Must select more than one user" users1
  *> validateUserDependence users2 users2

validateUsers2 :: Array Async.User -> Array Async.User -> V FormErrors (ValidatedArray Async.User)
validateUsers2 users2 users1 =
  Bifunctor.bimap (toMap FailUsers2) ValidatedArray
  $ validateNonEmptyArr "Users2 cannot be empty" users2
  *> validateUserDependence users2 users1

validateUserDependence :: Array Async.User -> Array Async.User -> V ValidationErrors (Array Async.User)
validateUserDependence users1 users2 =
  validateDependence
    (\u1 u2 -> Array.length u1 + (Array.length u2) > 4)
    users1
    users2
    "Users 1 and 2 must combine to 5 or more"

validateEmail :: String -> V FormErrors Email
validateEmail email =
  Bifunctor.bimap (toMap FailEmail) Email
  $  validateNonEmptyStr "Email cannot be empty" email
  *> validateStrIsEmail "Must be a valid email" email

validateUsername :: String -> V FormErrors Username
validateUsername uname =
  Bifunctor.bimap (toMap FailUsername) (Username <<< String.Utils.fromCharArray)
  $  validateNonEmptyStr "Username cannot be empty" uname
  *> validateMinLength 8 "Username must be longer than 8 characters" (String.Utils.toCharArray uname)

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
instance showFormErrorF :: Show (FormErrorKey) where
  show = Generic.Show.genericShow

type FormErrors = Map.Map FormErrorKey ValidationErrors

toMap :: FormErrorKey -> ValidationErrors -> FormErrors
toMap key errors = Map.fromFoldable $ Array.singleton $ Tuple key errors
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
          { label: "Developers"
          , helpText: Just "There are lots of developers to choose from."
          , valid: Map.lookup FailDevelopers st.validation
          }
          ( HH.slot' CP.cp1 unit TA.component (TAInput.defaultMulti' testFuzzyConfig testRecords) (HE.input HandleA) )
        , FormControl.formControl
          { label: "Todos"
          , helpText: Just "Synchronous todo fetching like you've always wanted."
          , valid: Map.lookup FailTodos st.validation
          }
          ( HH.slot' CP.cp2 unit TA.component (TAInput.defaultAsyncMulti' Async.todoFuzzyConfig Async.todos) (HE.input HandleB) )
        , FormControl.formControl
          { label: "Users"
          , helpText: Just "Oh, you REALLY need async, huh."
          , valid: Map.lookup FailUsers1 st.validation
          }
          ( HH.slot' CP.cp3 unit TA.component (TAInput.defaultContAsyncMulti' Async.userFuzzyConfig Async.users) (HE.input HandleC) )
        , FormControl.formControl
          { label: "Users 2"
          , helpText: Just "Honestly, this is just lazy."
          , valid: Map.lookup FailUsers2 st.validation
          }
          ( HH.slot' CP.cp4 unit TA.component (TAInput.defaultAsyncMulti' Async.userFuzzyConfig Async.users) (HE.input HandleD) )
        , FormControl.formControl
          { label: "Email"
          , helpText: Just "Dave will spam your email with gang of four patterns"
          , valid: Map.lookup FailEmail st.validation
          }
          ( Input.input
            [ HP.placeholder "davelovesgangoffour@gmail.com"
            , HE.onValueInput (HE.input $ UpdateTextField 1) ] )
        , FormControl.formControl
          { label: "Username"
          , helpText: Just "Put your name in and we'll spam you forever"
          , valid: Map.lookup FailUsername st.validation
          }
          ( Input.input
            [ HP.placeholder "Placehold me"
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

testRecords :: Array TestRecord
testRecords =
  [ TestRecord { name: "Chris", id: 0 }
  , TestRecord { name: "Dave", id: 1 }
  , TestRecord { name: "Thomas", id: 2 }
  , TestRecord { name: "Forest", id: 3 }
  ]

testFuzzyConfig :: { renderKey :: String, toStrMap :: TestRecord -> StrMap String }
testFuzzyConfig =
  { renderKey: "name"
  , toStrMap: \(TestRecord { name, id }) -> fromFoldable [ Tuple "name" name, Tuple "id" (show id) ]
  }
