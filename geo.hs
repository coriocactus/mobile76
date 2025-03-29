import qualified Control.Monad as Monad
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified System.Random as Random

type LatLong = (Double, Double)
type Polygon = [LatLong]
type Day = Int
type GeoStore = Map.Map Day [LatLong]
type Name = String

data Person = Person
  { name     :: Name
  , geoStore :: GeoStore
  } deriving (Show)

orientation :: LatLong -> LatLong -> LatLong -> Int
orientation p q r =
  let (px, py) = p
      (qx, qy) = q
      (rx, ry) = r
      val = (qy - py) * (rx - qx) - (qx - px) * (ry - qy)
  in if val > 0 then 1 else if val < 0 then -1 else 0

sqDist :: LatLong -> LatLong -> Double
sqDist (x1, y1) (x2, y2) = (x2 - x1)^2 + (y2 - y1)^2

convexHull :: [LatLong] -> [LatLong]
convexHull points
  | length points < 3 = points
  | otherwise =
      let p0 = List.minimumBy (\(x1, y1) (x2, y2) -> compare y1 y2 `mappend` compare x1 x2) points
          sortedPoints = List.sortBy (\p1 p2 ->
            let o = orientation p0 p1 p2
            in if o == 0 then compare (sqDist p0 p2) (sqDist p0 p1) else compare o 0)
            (List.delete p0 points)
          buildHull acc [] = acc
          buildHull acc (p:ps) =
            case acc of
              (a2:a1:as) ->
                if orientation a2 a1 p <= 0
                  then buildHull (a1:as) (p:ps)   -- Remove last point if clockwise or colinear
                  else buildHull (p:acc) ps       -- Add point if counterclockwise
              _ -> buildHull (p:acc) ps           -- Fewer than 2 points, just add
      in case sortedPoints of
           [] -> [p0]                             -- Only one point after deleting p0 (unlikely with length >= 3)
           (p1:ps) -> reverse $ buildHull [p0, p1] ps

triangulateConvex :: [LatLong] -> [(LatLong, LatLong, LatLong)]
triangulateConvex points
  | length points < 3 = []
  | otherwise =
      case points of
        (p0:rest) -> case rest of
          (p1:ps) -> zipWith (\a b -> (p0, a, b)) rest ps
          _ -> [] -- Fewer than 2 points after p0, no triangles possible
        _ -> []   -- Empty list, no triangles

triangleArea :: LatLong -> LatLong -> LatLong -> Double
triangleArea (x1, y1) (x2, y2) (x3, y3) = abs $ (x1 * (y2 - y3) + x2 * (y3 - y1) + x3 * (y1 - y2)) / 2

randomPointInTriangle :: (LatLong, LatLong, LatLong) -> IO LatLong
randomPointInTriangle (a, b, c) = do
  r1 <- Random.randomIO :: IO Double
  r2 <- Random.randomIO :: IO Double
  let s = sqrt r1          -- Ensures uniform distribution
      t = r2
      u = 1 - s            -- Barycentric coordinate for vertex a
      v = s * (1 - t)      -- Barycentric coordinate for vertex b
      w = s * t            -- Barycentric coordinate for vertex c
      (ax, ay) = a
      (bx, by) = b
      (cx, cy) = c
      x = u * ax + v * bx + w * cx
      y = u * ay + v * by + w * cy
  return (x, y)

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

mkBoundingBox :: Polygon -> [LatLong]
mkBoundingBox polygon = convexHull polygon

randomPointInConvexPolygon :: [LatLong] -> IO LatLong
randomPointInConvexPolygon points = do
  let triangles = triangulateConvex points
      areas = map (\(a, b, c) -> triangleArea a b c) triangles
      totalArea = sum areas
      cumulativeAreas = scanl1 (+) areas
  r <- Random.randomRIO (0, totalArea)
  let index = length $ takeWhile (< r) cumulativeAreas
      selectedTriangle = triangles !! index
  randomPointInTriangle selectedTriangle

randomPointInPolygon :: Polygon -> IO LatLong
randomPointInPolygon polygon = do
  point <- randomPointInConvexPolygon (mkBoundingBox polygon)
  if isInside point polygon then return point else randomPointInPolygon polygon

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
createPeopleWithGeoStores = Monad.mapM (\(name, specs) -> createPersonWithGeoStore name specs)

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
      peopleSpecs =
        [ ("Alice", [(day, 3) | day <- weekdays])
        , ("Bob", [(day, 3) | day <- weekdays])
        , ("Charlie", [(day, 3) | day <- weekdays])
        ]
  people <- createPeopleWithGeoStores peopleSpecs
  Monad.forM_ people displayPersonGeoStore
