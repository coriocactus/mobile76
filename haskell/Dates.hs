module Dates (
  testDates
             ) where

import Data.Time.Clock (
    getCurrentTime
                       )

import Data.Time.Format (
    formatTime,
    defaultTimeLocale
                        )

testDates :: IO ()
testDates = do
    currentTime <- getCurrentTime
    putStrLn $ "Current UTC time: " ++ show currentTime
    print currentTime

    let formattedTime = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" currentTime
    putStrLn $ "Formatted time: " ++ formattedTime

