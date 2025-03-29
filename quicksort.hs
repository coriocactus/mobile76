quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallerSorted = quicksort [a | a <- xs, a <= x]
      biggerSorted = quicksort [a | a <- xs, a > x]
  in  smallerSorted ++ [x] ++ biggerSorted

main :: IO ()
main = do
  let unsortedList = [3, 1, 4, 1, 5, 9, 2, 6, 5]
  putStrLn "Unsorted list:"
  print unsortedList
  putStrLn "Sorted list:"
  print (quicksort unsortedList)
