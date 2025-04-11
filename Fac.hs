fac :: (Num a, Eq a) => a -> a
fac n = go n 1
  where
    go :: (Num a, Eq a) => a -> a -> a
    go 0 acc = acc
    go m acc = go (m-1) (m*acc)

sneakySquare :: Double -> Double -> Double
sneakySquare x y
  | x > y     = x^2 + y^2
  | otherwise = x^2 - y^2

main :: IO ()
main= do
  print $ fac 5
  print $ sneakySquare 5.5 6.6
  print $ sneakySquare 6 5
