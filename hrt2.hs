import qualified Data.Map.Strict as Map

solution :: [Int] -> [[Int]] -> Bool
solution arr pieces =
  let pieceMap =
        Map.fromList
          [ (firstElem p, p)
            | p <- pieces,
            let firstElem (x:_) = x
                firstElem [] = error "Empty piece"
          ]
      match [] [] = True
      match [] _ = False
      match _ [] = False
      match (x:xs) remainingPieces =
        case Map.lookup x pieceMap of
          Just piece ->
            let pieceLength = length piece
            in take pieceLength (x:xs) == piece &&
                match (drop pieceLength (x:xs))
                      (filter (/= piece) remainingPieces)
          Nothing -> False
  in match arr pieces

main :: IO ()
main = do
  putStrLn "Enter the array (space-separated integers):"
  arrInput <- getLine
  let arr = map read (words arrInput) :: [Int]

  putStrLn "Enter number of pieces:"
  n <- readLn :: IO Int

  putStrLn "Enter each piece (one per line, space-separated integers):"
  pieces <- sequence $ replicate n $ do
    pieceInput <- getLine
    return (map read (words pieceInput) :: [Int])

  let result = solution arr pieces
  putStrLn $ "Result: " ++ show result
