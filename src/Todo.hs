{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Todo (TodoController, todoController, Todos) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (FromJSON, ToJSON)
import Data.IORef (IORef, modifyIORef, readIORef)
import GHC.Generics (Generic)
import Servant

data Todo = Todo
  { id :: Int,
    title :: String,
    done :: Bool
  }
  deriving (Generic, Show)

instance ToJSON Todo

instance FromJSON Todo

type Todos = IORef [Todo]

type TodoController = "todos" :> (Get '[JSON] [Todo] :<|> ReqBody '[JSON] Todo :> Post '[JSON] Todo)

listTodos :: Todos -> Handler [Todo]
listTodos = liftIO . readIORef

createTodo :: Todos -> Todo -> Handler Todo
createTodo todosRef todo = liftIO $ do
  modifyIORef todosRef (todo :)
  return todo

todoController :: Todos -> Server TodoController
todoController todosRef = listTodos todosRef :<|> createTodo todosRef
