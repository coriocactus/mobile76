{-# LANGUAGE OverloadedStrings #-}

import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200, status404)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll, recvFrom, sendTo)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import Control.Concurrent (forkIO)
import Control.Concurrent.Async (concurrently_)
import Control.Exception (catch, SomeException)
import Control.Monad (forever)

app :: Application
app req respond = respond $ case pathInfo req of
  [] -> responseStr status200 "packets!"
  _  -> responseStr status404 "Not found"
  where
    responseStr status msg = responseLBS status [("Content-Type", "text/plain")] (L.fromStrict (C.pack msg))

tcpServer :: PortNumber -> IO ()
tcpServer port = withSocketsDo $ do
  sock <- socket AF_INET Stream defaultProtocol
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet port (tupleToHostAddress (127, 0, 0, 1)))
  listen sock 5
  putStrLn $ "TCP server listening on port " ++ show port

  forever $ do
    (conn, addr) <- accept sock
    putStrLn $ "TCP connection from: " ++ show addr
    forkIO $ handleTcpClient conn
  where
    handleTcpClient conn = do
      msg <- recv conn 1024
      putStrLn $ "TCP received: " ++ C.unpack msg
      sendAll conn (C.pack ("Echo: " ++ C.unpack msg))
      close conn

udpServer :: PortNumber -> IO ()
udpServer port = withSocketsDo $ do
  sock <- socket AF_INET Datagram defaultProtocol
  bind sock (SockAddrInet port (tupleToHostAddress (127, 0, 0, 1)))
  putStrLn $ "UDP server listening on port " ++ show port

  forever $ do
    (msg, addr) <- recvFrom sock 1024
    putStrLn $ "UDP received from " ++ show addr ++ ": " ++ C.unpack msg
    sendTo sock (C.pack ("Echo: " ++ C.unpack msg)) addr

main :: IO ()
main = do
  putStrLn "Starting servers..."
  concurrently_
    (run 8080 app)      -- curl http://127.0.0.1:8080
    (concurrently_ 
      (tcpServer 9000)  -- nc 127.0.0.1 9000
      (udpServer 9050)  -- nc -u 127.0.0.1 9050
    )
