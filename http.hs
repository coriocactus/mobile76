import Network.Socket
import System.IO

main :: IO ()
main = withSocketsDo $ do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet 8080 (tupleToHostAddress(127,0,0,1)))
  listen sock 5
  putStrLn "Listening on port 8080"
  mainLoop sock

mainLoop :: Socket -> IO ()
mainLoop sock = do
  (conn, addr) <- accept sock
  handle <- socketToHandle conn ReadWriteMode
  request <- hGetLine handle
  putStrLn request
  putStrLn $ show addr
  hPutStr handle response
  hClose handle
  mainLoop sock

response :: String
response = "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\n\r\nHello, World!"

-- async
-- hot reload
