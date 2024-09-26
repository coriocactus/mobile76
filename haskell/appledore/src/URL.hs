module URL (
  testURL
           ) where

{-
    ___           ___                   
   /__/\         /  /\                  
   \  \:\       /  /::\                 
    \  \:\     /  /:/\:\    ___     ___ 
___  \  \:\   /  /:/~/:/   /__/\   /  /\
/__/\  \__\:\ /__/:/ /:/___ \  \:\ /  /:/
\  \:\ /  /:/ \  \:\/:::::/  \  \:\  /:/ 
\  \:\  /:/   \  \::/~~~~    \  \:\/:/  
 \  \:\/:/     \  \:\         \  \::/   
  \  \::/       \  \:\         \__\/    
   \__\/         \__\/                  

   -}

import Network.URI (
  parseURI, uriScheme, uriAuthority,
  uriUserInfo, uriRegName, uriPort,
  uriPath, uriFragment, uriQuery
                   )

testURL :: IO ()
testURL = do
  let s = "postgres://user:pass@host.com:5432/path?k=v#f"
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
