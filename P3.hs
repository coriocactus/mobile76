import Control.Exception

{-
  The prime factors of 13195 are 5, 7, 13 and 29.
  What is the largest prime factor of the number 600851475143?
-}

isPrime :: Int -> Bool
isPrime x
  | x == 1 = False
  | x == 2 || x == 3 = True
  | even x || mod x 3 == 0 = False
  | otherwise = False

largestPrimeFactor :: Int -> Int -> Int
largestPrimeFactor x y
  | mod x y == 0 && isPrime y = y
  | otherwise = largestPrimeFactor x (y - 1)

main :: IO ()
main = do
  putStrLn "Example: "
  assert (largestPrimeFactor 13195 13195 == 29) (putStr "Passed")
  putStrLn "Test Case: "
  print (largestPrimeFactor 600851475143 600851475143)
