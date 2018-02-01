module UIGuide.Utilities.Async where

import Prelude

import Data.Either (Either(..))
import Data.Argonaut (Json, decodeJson, (.?))
import Network.HTTP.Affjax (get, AJAX)
import Network.RemoteData (RemoteData(..))
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (log, CONSOLE)
import CN.UI.Core.Typeahead (class StringComparable)

import Data.Traversable (traverse)

----------
-- Async functions

fetchTodos :: forall e.
   Aff
     ( ajax :: AJAX
     , console :: CONSOLE
     | e
     )
     (RemoteData String (Array Todo))
fetchTodos = do
  res <- get "https://jsonplaceholder.typicode.com/todos"
  log "Fetching todos..."
  case decodeTodos res.response of
    Left s -> pure $ Failure s
    Right arr -> pure $ Success arr

----------
-- Todos from JSON API

newtype Todo = Todo
  { title :: String
  , completed :: Boolean }

derive instance eqTodo :: Eq Todo

instance showTodo :: Show Todo where
  show (Todo { title, completed }) = title <> " " <> show completed
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
