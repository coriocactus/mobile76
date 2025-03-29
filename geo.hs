import Control.Monad (forM)
import System.Random (randomRIO)

type LatLong = (Double, Double)
type BoundingBox = (LatLong, LatLong)

randomLatLong :: BoundingBox -> IO LatLong
randomLatLong ((minLat, minLng), (maxLat, maxLng)) = do
  lat <- randomRIO (minLat, maxLat)
  lng <- randomRIO (minLng, maxLng)
  return (lat, lng)

randomLondonLocation :: IO LatLong
randomLondonLocation =
  randomLatLong ((51.28676, -0.5103751), (51.6918741, 0.3340155))

manyRandomLocations :: Int -> IO [LatLong]
manyRandomLocations n = mapM (const randomLondonLocation) [1..n]

main :: IO ()
main = do
  coords <- manyRandomLocations 10
  putStrLn $ "London:" 
  forM coords $ \(lat, lng) -> do
    putStrLn $ show lat ++ ", " ++ show lng
