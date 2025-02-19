import Data.Char (digitToInt)

intToBinary :: Int -> String
intToBinary 0 = "0"
intToBinary n = reverse $ go n
    where
        go 0 = ""
        go n = let (q, r) = divMod n 2 in show r ++ go q

binaryToInt :: String -> Int
binaryToInt = foldl (\acc x -> acc * 2 + digitToInt x) 0

padToEight :: String -> String
padToEight x = replicate (8 - length x) '0' ++ x

solution a = binaryToInt $ concat $ reverse $ map (padToEight . intToBinary) a

main = do
    print $ padToEight $ intToBinary 24
    print $ solution [24, 85, 0]
