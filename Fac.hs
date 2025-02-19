module Fac (
  testFac
) where

fac :: Integer -> Integer
fac 0 = 1
fac n = n * fac (n-1)

sneakySquare :: Double -> Double -> Double
sneakySquare x y | x > y      = x ^ (2 :: Integer) + y ^ (2 :: Integer)
                 | otherwise  = x ^ (2 :: Integer)  - y ^ (2 :: Integer)

testFac :: IO ()
testFac = do
  print $ fac 5
  print $ sneakySquare 5.5 6.6
  print $ sneakySquare 6 5
