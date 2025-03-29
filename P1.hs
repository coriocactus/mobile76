import Control.Exception

{-
  If we list all the natural numbers below that are multiples of 3 or 5,
  we get 3, 5, 6, and 9. The sum of these multiples is 23.
  Find the sum of all the multiples of 3 or 5 below 1000. 
-}

multipleOfThreeOrFive :: Int -> Int -> Int -> Int
multipleOfThreeOrFive x y z
  | x == 1 = 0
  | mod x y == 0 || mod x z == 0 = x + multipleOfThreeOrFive (x-1) y z
  | otherwise = multipleOfThreeOrFive (x-1) y z

main :: IO ()
main = do
  putStr "Example: "
  assert ((multipleOfThreeOrFive 9 3 5) == 23) (putStrLn "Passed!")
  putStr "Test Case: "
  print (multipleOfThreeOrFive 999 3 5)
