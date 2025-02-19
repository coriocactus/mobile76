{-

ask :: Reader a a
ask = Reader id

asks :: (r -> a) -> Reader r a
asks f = Reader f

    -}

import Control.Monad.Reader

exampleReader :: Reader String String
exampleReader = do
  env <- ask
  return (env ++ " - This is the environment.")

exampleReader2 :: Reader String String
exampleReader2 = ask >>= (\env -> return (env ++ " - This is the environment."))

result = runReader exampleReader "Hello"
