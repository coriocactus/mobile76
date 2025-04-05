{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Control.Monad.IO.Class as MonadIO
import qualified Data.Text as Text

import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

import Servant (Get)
import Servant (PlainText)
import Servant ((:>), (:<|>)(..))
import Servant as Servant

-- ===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|
-- | API(+[ENDPOINT]) <-> SERVER(+[HANDLER])

type GreetAPI =
  "hello" :> Get '[PlainText] Text.Text :<|>
  "bye"   :> Get '[PlainText] Text.Text

greetServant :: Servant.Server GreetAPI
greetServant =
  handleHello :<|>
  handleBye
  where
    handleHello :: Servant.Handler Text.Text
    handleHello = do
      MonadIO.liftIO $ putStrLn "Received request for /hello"
      return "Hello from Servant!"

    handleBye :: Servant.Handler Text.Text
    handleBye = do
      MonadIO.liftIO $ putStrLn "Received request for /bye"
      return "Goodbye from Servant!"

-- ===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|
-- | +[API(+[ENDPOINT]) <-> SERVER(+[HANDLER])] + PROXY -> APPLICATION

type API = GreetAPI :<|> Servant.EmptyAPI

servant :: Servant.Application
servant = Servant.serve translator servants
  where
    translator :: Servant.Proxy API
    translator = Servant.Proxy

    servants :: Servant.Server API
    servants = greetServant :<|> Servant.emptyServer

-- ===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|
-- | MIDDLEWARE

notFoundMiddleware :: Wai.Middleware
notFoundMiddleware app req respond = app req $ \response ->
  case Wai.responseStatus response of
    status
      | status == HTTP.status404 -> do
          MonadIO.liftIO $ putStrLn $ "Not Found: " ++ show (Wai.rawPathInfo req)
          respond $ Wai.responseLBS
            HTTP.status404
            [("Content-Type", "text/plain")]
            "Not Found: The requested resource does not exist."
    _ -> respond response

application :: Wai.Application
application = notFoundMiddleware servant

-- ===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|
-- | MAIN

main :: IO ()
main = do
  let port = 8080
  putStrLn $ "Starting server on http://localhost:" ++ show port ++ "/hello"
  putStrLn $ "Starting server on http://localhost:" ++ show port ++ "/bye"
  Warp.run port application
