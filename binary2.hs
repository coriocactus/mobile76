import Data.Maybe (listToMaybe)

solution n =
  case findFirstZeroPosition (intToBinary n) of
    Just pos -> 2 ^ pos
    Nothing -> 0  -- Handle case where no zeros exist

findFirstZeroPosition :: String -> Maybe Int
findFirstZeroPosition binary =
  listToMaybe
    [ read idx :: Int
    | (idx, bit) <- zip (map show [0..]) (reverse binary), bit == '0'
    ]

intToBinary 0 = "0"
intToBinary n = reverse $ go n
  where
    go 0 = ""
    go n = let (q, r) = divMod n 2 in show r ++ go q

main = do
  case findFirstZeroPosition (intToBinary 1073741824) of
    Just pos -> print pos
    Nothing -> putStrLn "No zeros found in binary representation"
