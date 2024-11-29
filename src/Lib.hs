{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Lib
  ( runServer,
  )
where

import Data.IORef (newIORef)
import Network.Wai.Handler.Warp (run)
import Servant
import Todo (TodoController, Todos, todoController)

type API = "hello" :> Get '[PlainText] String :<|> TodoController

helloHandler :: String
helloHandler = "Hello, world!"

server :: Todos -> Server API
server todosRef = return helloHandler :<|> todoController todosRef

api :: Proxy API
api = Proxy

app :: Todos -> Application
app todos = serve api (server todos)

runServer :: IO ()
runServer = do
  todosRef <- newIORef []
  run 8080 (app todosRef)
