{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Control.Monad.IO.Class as MonadIO
import qualified Data.Text as Text

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

greetServer :: Servant.Server GreetAPI
greetServer =
  helloHandler :<|>
  byeHandler

helloHandler :: Servant.Handler Text.Text
helloHandler = do
  MonadIO.liftIO $ putStrLn "Received request for /hello"
  return "Hello from Servant!"

byeHandler :: Servant.Handler Text.Text
byeHandler = do
  MonadIO.liftIO $ putStrLn "Received request for /bye"
  return "Goodbye from Servant!"

-- ===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|
-- | +[API(+[ENDPOINT]) <-> SERVER(+[HANDLER])] + PROXY -> APPLICATION

type API = GreetAPI :<|> Servant.EmptyAPI

server :: Servant.Server API
server = greetServer :<|> Servant.emptyServer

proxy :: Servant.Proxy API
proxy = Servant.Proxy

app :: Servant.Application
app = Servant.serve proxy server

-- ===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|
-- | MAIN

main :: IO ()
main = do
  let port = 8080
  putStrLn $ "Starting server on http://localhost:" ++ show port ++ "/hello"
  putStrLn $ "Starting server on http://localhost:" ++ show port ++ "/bye"
  Warp.run port app
