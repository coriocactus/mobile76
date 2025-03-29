import qualified Data.Time.Clock as Clock
import qualified Data.Time.Format as DateTimeFormat

main :: IO ()
main = do
  currentTime <- Clock.getCurrentTime
  putStrLn $ "Current UTC time: " ++ show currentTime
  print currentTime
  let hour = DateTimeFormat.formatTime DateTimeFormat.defaultTimeLocale "%H" currentTime
      hourlyVersion = ['a'..'z'] !! read hour
      dailyVersion = DateTimeFormat.formatTime DateTimeFormat.defaultTimeLocale "%Y-%m-%d" currentTime
      version = dailyVersion ++ [hourlyVersion]
  putStrLn $ "v" ++ dailyVersion ++ [hourlyVersion]
