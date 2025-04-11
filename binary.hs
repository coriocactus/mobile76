import Data.Char (digitToInt)
import Data.Bits (testBit, shiftL)

-- Convert Int to binary string representation
intToBinary :: Int -> String
intToBinary 0 = "0"
intToBinary n = reverse $ go n
  where
    go 0 = ""
    go n = let (q, r) = quotRem n 2 in (if r == 0 then '0' else '1') : go q

-- Convert binary string to Int
binaryToInt :: String -> Int
binaryToInt = foldl' (\acc x -> acc `shiftL` 1 + digitToInt x) 0
  where
    foldl' f z [] = z
    foldl' f z (x:xs) = let z' = f z x in z' `seq` foldl' f z' xs

-- Pad a string to length 8 with leading zeros
padToEight :: String -> String
padToEight xs = let len = length xs
                in if len < 8
                   then replicate (8 - len) '0' ++ xs
                   else xs

-- Convert list of Ints to binary, pad, reverse, concat, and convert back
solution :: [Int] -> Int
solution = binaryToInt . concat . reverse . map (padToEight . intToBinary)

main = do
  print $ padToEight $ intToBinary 24
  print $ solution [24, 85, 0]
