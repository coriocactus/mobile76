import qualified Control.Monad as Monad
import qualified System.Random as Random
import qualified Data.Map.Strict as Map

type LatLong = (Double, Double)
type Polygon = [LatLong]
type Day = Int
type GeoStore = Map.Map Day [LatLong]
type Name = String

data Person = Person
  { name     :: Name
  , geoStore :: GeoStore
  } deriving (Show)

isInside :: LatLong -> Polygon -> Bool
isInside point polygon = odd $ length intersections
  where
    (px, py) = point
    rayStart = (px, py)
    rayEnd = (1000000, py)
    intersections = filter (not . isNothing) $ zipWith (\p3 p4 -> lineIntersect (rayStart, rayEnd) p3 p4) polygon (rotate polygon)
    rotate [] = []
    rotate (x:xs) = xs ++ [x]
    isNothing Nothing = True
    isNothing _ = False

lineIntersect :: (LatLong, LatLong) -> LatLong -> LatLong -> Maybe LatLong
lineIntersect (p1, p2) p3 p4 =
    let (x1, y1) = p1
        (x2, y2) = p2
        (x3, y3) = p3
        (x4, y4) = p4
        den = (y4 - y3) * (x2 - x1) - (x4 - x3) * (y2 - y1)
        ua = ((x4 - x3) * (y1 - y3) - (y4 - y3) * (x1 - x3)) / den
        ub = ((x2 - x1) * (y1 - y3) - (y2 - y1) * (x1 - x3)) / den
    in if den == 0 then Nothing else
          if ua >= 0 && ua <= 1 && ub >= 0 && ub <= 1 then
            Just (x1 + ua * (x2 - x1), y1 + ua * (y2 - y1))
          else
            Nothing

mkBoundingBox :: Polygon -> (LatLong, LatLong)
mkBoundingBox polygon =
  let lats = map fst polygon
      lngs = map snd polygon
      minLat = minimum lats
      maxLat = maximum lats
      minLng = minimum lngs
      maxLng = maximum lngs
  in ((minLat, minLng), (maxLat, maxLng))

randomPointInPolygon :: Polygon -> IO LatLong
randomPointInPolygon polygon = do
  let boundingBox = mkBoundingBox polygon
  point <- randomLatLong boundingBox
  if isInside point polygon then
    return point
  else
    randomPointInPolygon polygon

randomLatLong :: (LatLong, LatLong) -> IO LatLong
randomLatLong ((minLat, minLng), (maxLat, maxLng)) = do
  lat <- Random.randomRIO (minLat, maxLat)
  lng <- Random.randomRIO (minLng, maxLng)
  return (lat, lng)

londonPolygon :: Polygon
londonPolygon = [
  (51.28676, -0.5103751),
  (51.28676, 0.3340155),
  (51.6918741, 0.3340155),
  (51.6918741, -0.5103751)
  ]

randomLondonLocation :: IO LatLong
randomLondonLocation = randomPointInPolygon londonPolygon

manyRandomLondonLocations :: Int -> IO [LatLong]
manyRandomLondonLocations n = mapM (const randomLondonLocation) [1..n]

createDayGeoStore :: Int -> IO [LatLong]
createDayGeoStore = manyRandomLondonLocations

createGeoStore :: [(Day, Int)] -> IO GeoStore
createGeoStore daySpecs = do
  entries <- Monad.mapM
    ( \(day, n) -> do
        locations <- createDayGeoStore n
        return (day, locations)
    ) daySpecs
  return $ Map.fromList entries

createPersonWithGeoStore :: Name -> [(Day, Int)] -> IO Person
createPersonWithGeoStore name daySpecs = do
  store <- createGeoStore daySpecs
  return $ Person name store

createPeopleWithGeoStores :: [(Name, [(Day, Int)])] -> IO [Person]
createPeopleWithGeoStores =
  Monad.mapM (\(name, specs) -> createPersonWithGeoStore name specs)

dayName :: Day -> String
dayName 1 = "Monday"
dayName 2 = "Tuesday"
dayName 3 = "Wednesday"
dayName 4 = "Thursday"
dayName 5 = "Friday"
dayName 6 = "Saturday"
dayName 7 = "Sunday"

displayPersonGeoStore :: Person -> IO ()
displayPersonGeoStore person = do
  putStrLn $ name person ++ "'s locations:"
  Monad.forM_ (Map.toList $ geoStore person) $ \(day, locations) -> do
    putStrLn $ "  " ++ dayName day ++ ":"
    Monad.forM_ locations $ \(lat, lng) -> putStrLn $ "    " ++ show lat ++ ", " ++ show lng

main :: IO ()
main = do
  let weekdays = [1..7]
      peopleSpecs = [
        ("Alice", [(day, 3) | day <- weekdays]),
        ("Bob", [(day, 3) | day <- weekdays]),
        ("Charlie", [(day, 3) | day <- weekdays])
        ]
  people <- createPeopleWithGeoStores peopleSpecs
  Monad.forM_ people displayPersonGeoStore
