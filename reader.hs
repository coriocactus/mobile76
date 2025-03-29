import Control.Monad.Reader

-- ask :: Reader a a
-- ask = Reader id
-- 
-- asks :: (r -> a) -> Reader r a
-- asks f = Reader f

exampleReader :: Reader String String
exampleReader = do
  env <- ask
  return (env ++ " - This is the environment.")

exampleReader2 :: Reader String String
exampleReader2 = ask >>= (\env -> return (env ++ " - This is the environment."))

main :: IO ()
main = do
  putStrLn $ runReader exampleReader "Hello 1"
  putStrLn $ runReader exampleReader2 "Hello 2"
  putStrLn $ runReader (asks (++ " - This is the environment.")) "Hello 3"
