solution n =
  binaryToInt $ concat
  $ [ [fst x] ++ [snd x] | x <- binaryPairs $ padToEight $ intToBinary n ]

intToBinary 0 = "0"
intToBinary n = reverse $ go n
    where
      go 0 = ""
      go n = let (q, r) = divMod n 2 in show r ++ go q
                                                        
binaryPairs [] = []
binaryPairs (x:y:z) = [(y,x)] ++ binaryPairs z

padToEight x = replicate (8 - length x) '0' ++ x

binaryToInt :: String -> Int
binaryToInt = foldl (\x y -> x * 2 + (read [y] :: Int)) 0

main = print $ padToEight $ intToBinary 166680
