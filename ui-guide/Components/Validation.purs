module UIGuide.Components.Validation where

import Prelude

import CN.UI.Block.Button as Button
import CN.UI.Block.FormControl as FormControl
import CN.UI.Block.Input as Input
import CN.UI.Components.Typeahead as TAInput
import CN.UI.Core.Typeahead as TA
import Control.Alt ((<|>))
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Aff.Console (CONSOLE, logShow)
import Control.Monad.Eff.Timer (TIMER)
import DOM (DOM)
import Data.Array (length)
import Data.Bifunctor as Bifunctor
import Data.Either (fromRight)
import Data.Either.Nested (Either4)
import Data.Foldable as Foldable
import Data.Functor.Coproduct.Nested (Coproduct4)
import Data.Generic.Rep as Generic
import Data.Generic.Rep.Eq as Generic.Eq
import Data.Generic.Rep.Show as Generic.Show
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Semiring.Free (Free, free)
import Data.StrMap (StrMap, fromFoldable)
import Data.String as String
import Data.String.Regex as Regex
import Data.String.Regex.Flags as Regex.Flags
import Data.String.Utils as String.Utils
import Data.Tuple (Tuple(..))
import Data.Validation.Semiring (V, unV, invalid, isValid)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax (AJAX)
import Partial.Unsafe (unsafePartial)
import UIGuide.Block.Component as Component
import UIGuide.Block.Documentation as Documentation
import UIGuide.Utilities.Async as Async

----------
-- Component Types

type State =
  { raw :: UnvalidatedForm
  , validation :: Maybe (V FormErrors ValidatedForm)
  }

data Query a
  = NoOp a
  | HandleA (TA.TypeaheadMessage Query TestRecord Void Void) a
  | HandleB (TA.TypeaheadMessage Query Async.Todo (Async.Source Async.Todo) Async.Err) a
  | HandleC (TA.TypeaheadMessage Query Async.User (Async.Source Async.User) Async.Err) a
  | HandleD (TA.TypeaheadMessage Query Async.User (Async.Source Async.User) Async.Err) a
  | UpdateTextField Int String a
  | FormSubmit a


----------
-- Child paths

type ChildSlot = Either4 Unit Unit Unit Unit
type ChildQuery eff m = Coproduct4
  (TA.TypeaheadQuery Query TestRecord Void Void eff m)
  (TA.TypeaheadQuery Query Async.Todo (Async.Source Async.Todo) Async.Err eff m)
  (TA.TypeaheadQuery Query Async.User (Async.Source Async.User) Async.Err eff m)
  (TA.TypeaheadQuery Query Async.User (Async.Source Async.User) Async.Err eff m)


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
      , validation: Nothing }
  , render
  , eval
  , receiver: const Nothing
  }
  where
    render
      :: State
      -> H.ParentHTML Query (ChildQuery (Effects eff) m) ChildSlot m
    render st = HH.div_ [ renderForm, renderValidation st ]

    eval
      :: Query
      ~> H.ParentDSL State Query (ChildQuery (Effects eff) m) ChildSlot Void m
    eval (NoOp next) = pure next

    -- Done asynchronously so data can load in the background.
    eval (UpdateTextField i str next) = case i of
      1 -> H.modify (_ { raw { email = str }}) *> pure next
      2 -> H.modify (_ { raw { username = str }}) *> pure next
      _ -> pure next

    eval (HandleA message next) = case message of
      _ -> pure next

    eval (HandleB message next) = case message of
      TA.RequestData syncMethod -> do
         _ <- H.fork do
          res <- H.liftAff $ Async.load syncMethod
          case TA.maybeReplaceItems res syncMethod of
            Nothing -> pure Nothing
            (Just newSync) -> do
               _ <- H.query' CP.cp2 unit $ H.action $ TA.FulfillRequest newSync
               pure Nothing
         pure next
      _ -> pure next

    eval (HandleC message next) = case message of
      TA.RequestData syncMethod -> do
         _ <- H.fork do
          res <- H.liftAff $ Async.load syncMethod
          case TA.maybeReplaceItems res syncMethod of
            Nothing -> pure Nothing
            (Just newSync) -> do
               _ <- H.query' CP.cp3 unit $ H.action $ TA.FulfillRequest newSync
               pure Nothing
         pure next
      _ -> pure next

    eval (HandleD message next) = case message of
      TA.RequestData syncMethod -> do
         _ <- H.fork do
          res <- H.liftAff $ Async.load syncMethod
          case TA.maybeReplaceItems res syncMethod of
            Nothing -> pure Nothing
            (Just newSync) -> do
               _ <- H.query' CP.cp4 unit $ H.action $ TA.FulfillRequest newSync
               pure Nothing
         pure next
      _ -> pure next

    eval (FormSubmit next) = do
      -- Collect data from components
      devs <- H.query' CP.cp1 unit (H.request TA.Selections)
      todos <- H.query' CP.cp2 unit (H.request TA.Selections)
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
      H.modify (_ { validation = Just $ runValidation st.raw })
      pure next


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
  <$> (Bifunctor.lmap free $ validateDevelopers f.developers)
  <*> (Bifunctor.lmap free $ validateTodos f.todos)
  <*> (Bifunctor.lmap free $ validateUsers1 f.users1)
  <*> (Bifunctor.lmap free $ validateUsers2 f.users2)
  <*> (Bifunctor.lmap free $ validateEmail f.email)
  <*> (Bifunctor.lmap free $ validateUsername f.username)

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

validateDevelopers :: Array TestRecord -> V FormError (ValidatedArray TestRecord)
validateDevelopers xs =
  Bifunctor.bimap FailDevelopers ValidatedArray
  $ validateMinLength 2 xs

validateTodos :: Array Async.Todo -> V FormError (ValidatedArray Async.Todo)
validateTodos xs =
  Bifunctor.bimap FailTodos ValidatedArray
  $ validateNonEmptyArr xs

validateUsers1 :: Array Async.User -> V FormError (ValidatedArray Async.User)
validateUsers1 xs =
  Bifunctor.bimap FailUsers1 ValidatedArray
  $ validateNonEmptyArr xs
  *> validateMinLength 2 xs

validateUsers2 :: Array Async.User -> V FormError (ValidatedArray Async.User)
validateUsers2 xs =
  Bifunctor.bimap FailUsers2 ValidatedArray
  $ validateNonEmptyArr xs

validateEmail :: String -> V FormError Email
validateEmail email =
  Bifunctor.bimap FailEmail Email
  $  validateNonEmptyStr email
  *> validateEmailRegex email

validateUsername :: String -> V FormError Username
validateUsername uname =
  Bifunctor.bimap FailUsername (Username <<< String.Utils.fromCharArray)
  $  validateNonEmptyStr uname
  *> validateMinLength 8 (String.Utils.toCharArray uname)

-----
-- Specialized types

newtype Email = Email String
newtype Username = Username String
newtype ValidatedArray a = ValidatedArray (Array a)

-----
-- Form types

data FormErrorF a
  = FailDevelopers a
  | FailTodos a
  | FailUsers1 a
  | FailUsers2 a
  | FailEmail a
  | FailUsername a

derive instance functorFormErrorF :: Functor FormErrorF
derive instance genericFormErrorF :: Generic.Generic (FormErrorF a) _
instance showFormErrorF :: Show a => Show (FormErrorF a) where
  show = Generic.Show.genericShow

type FormError = FormErrorF ValidationErrors
type FormErrors = Free FormError

-----
-- Validation types

type ValidationErrors = Free ValidationError

data ValidationError
  = EmptyField
  | InvalidEmail
  | UnderMinLength

derive instance genericValidationError :: Generic.Generic ValidationError _

instance eqValidationError :: Eq ValidationError where
  eq = Generic.Eq.genericEq

instance showValidationError :: Show ValidationError where
  show = Generic.Show.genericShow

-----
-- Possible validations to run on any field

validateNonEmptyStr :: String -> V ValidationErrors String
validateNonEmptyStr str
  | String.null str = invalid $ free EmptyField
  | otherwise = pure str

validateNonEmptyArr :: ∀ a. Array a -> V ValidationErrors (Array a)
validateNonEmptyArr [] = invalid $ free EmptyField
validateNonEmptyArr xs = pure xs

validateEmailRegex :: String -> V ValidationErrors String
validateEmailRegex email
  | Regex.test emailRegex email = pure email
  | otherwise = invalid $ free InvalidEmail

validateMinLength :: ∀ f a. Foldable.Foldable f => Int -> f a -> V ValidationErrors (f a)
validateMinLength n f
  | Foldable.length f >= n = pure f
  | otherwise = invalid $ free UnderMinLength



-----
-- Regexes to use in running validations

unsafeRegexFromString :: String -> Regex.Regex
unsafeRegexFromString str =
  let regex = Regex.regex str Regex.Flags.noFlags
   in unsafePartial $ fromRight regex

emailRegex :: Regex.Regex
emailRegex = unsafeRegexFromString "^\\w+([.-]?\\w+)*@\\w+([.-]?\\w+)*(\\.\\w{2,3})+$"



----------
-- Rendering

renderForm :: ∀ eff m
  . MonadAff (Effects eff) m
 => H.ParentHTML Query (ChildQuery (Effects eff) m) ChildSlot m
renderForm =
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
          }
          ( HH.slot' CP.cp1 unit TA.component (TAInput.defaultMulti testRecords testToStrMap renderFuzzy renderItem) (HE.input HandleA) )
        , FormControl.formControl
          { label: "Todos"
          , helpText: Just "Synchronous todo fetching like you've always wanted."
          }
          ( HH.slot' CP.cp2 unit TA.component (TAInput.defaultAsyncMulti Async.todos Async.todoToStrMap Async.todoRenderFuzzy Async.todoRenderItem) (HE.input HandleB) )
        , FormControl.formControl
          { label: "Users"
          , helpText: Just "Oh, you REALLY need async, huh."
          }
          ( HH.slot' CP.cp3 unit TA.component (TAInput.defaultContAsyncMulti Async.users Async.userToStrMap Async.userRenderFuzzy Async.userRenderItem) (HE.input HandleC) )
        , FormControl.formControl
          { label: "Users 2"
          , helpText: Just "Honestly, this is just lazy."
          }
          ( HH.slot' CP.cp4 unit TA.component (TAInput.defaultAsyncMulti Async.users Async.userToStrMap Async.userRenderFuzzy Async.userRenderItem) (HE.input HandleD) )
        , FormControl.formControl
          { label: "Email"
          , helpText: Just "Dave will spam your email with gang of four patterns"
          }
          ( Input.input
            [ HP.placeholder "davelovesgangoffour@gmail.com"
            , HE.onValueInput (HE.input $ UpdateTextField 1) ] )
        , FormControl.formControl
          { label: "Username"
          , helpText: Just "Put your name in and we'll spam you forever"
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

renderValidation :: ∀ eff m
  . MonadAff (Effects eff) m
 => State
 -> H.ParentHTML Query (ChildQuery (Effects eff) m) ChildSlot m
renderValidation st = case st.validation of
  Nothing -> HH.div_ []
  Just v  ->
    HH.div
    [ HP.class_ $ HH.ClassName "mt-4 p-4 bg-grey-lightest font-mono" ]
    [ showV v ]

  where
    renderLine x = HH.p [ HP.class_ $ HH.ClassName "py-1" ] [ HH.text x ]
    stringify = unV show (const "")
    showV = renderLine <<< stringify


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

testToStrMap :: TestRecord -> StrMap String
testToStrMap (TestRecord { name, id }) =
  fromFoldable [ Tuple "name" name, Tuple "id" (show id) ]

renderFuzzy :: ∀ o. TAInput.RenderContainerRow o TestRecord
renderFuzzy = TAInput.defaultContainerRow (TAInput.boldMatches "name")

renderItem :: ∀ o source err eff m
  . TestRecord -> TAInput.TAParentHTML o TestRecord source err eff m
renderItem = TAInput.defaultSelectionRow $ HH.text <<< _.name <<< unwrap
