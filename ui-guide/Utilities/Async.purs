module UIGuide.Utilities.Async where

import Prelude

import Data.Either (Either(..))
import Data.Argonaut (Json, decodeJson, (.?))
import Network.HTTP.Affjax (get, AJAX)
import Control.Monad.Aff (Aff)
import CN.UI.Core.Typeahead (class StringComparable)

import Data.Traversable (traverse)

----------
-- Async functions

fetchTodos :: forall e.
   Aff
     ( ajax :: AJAX
     | e
     )
     (Array Todo)
fetchTodos = do
  res <- get "https://jsonplaceholder.typicode.com/todos"
  case decodeTodos res.response of
    Left s -> pure []
    Right arr -> pure arr

----------
-- Todos from JSON API

newtype Todo = Todo
  { title :: String
  , completed :: Boolean }

derive instance eqTodo :: Eq Todo
instance stringComparableTodo :: StringComparable Todo where
  toString (Todo { title, completed }) = title <> " " <> show completed

decodeTodos :: Json -> Either String (Array Todo)
decodeTodos json = decodeJson json >>= traverse decodeTodo

decodeTodo :: Json -> Either String Todo
decodeTodo json = do
  obj <- decodeJson json
  title <- obj .? "title"
  completed <- obj .? "completed"
  pure $ Todo { title, completed }
