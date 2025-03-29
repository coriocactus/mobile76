import Network.URI

main :: IO ()
main = do
  let s = "https://github.com/cordcivilian/cord.git"
  case parseURI s of
    Nothing  -> error "no URI"
    Just uri -> do
      putStrLn $ uriScheme uri
      case uriAuthority uri of
        Nothing   -> error "no Authority"
        Just auth -> do
          putStrLn $ uriUserInfo auth
          putStrLn $ uriRegName auth
          putStrLn $ uriPort auth
      putStrLn $ Network.URI.uriPath uri
      putStrLn $ Network.URI.uriFragment uri
      putStrLn $ Network.URI.uriQuery uri
