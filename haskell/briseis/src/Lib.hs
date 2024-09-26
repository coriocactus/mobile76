{-# LANGUAGE OverloadedStrings #-} 

module Lib (
    someFunc,
    anotherFunc
) where

import qualified Data.Text.IO as T 

someFunc :: IO ()
someFunc = T.putStrLn "someFunc" 

anotherFunc:: IO ()
anotherFunc = T.putStrLn "anotherFunc" 
