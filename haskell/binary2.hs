solution n = 2 ^ (head $ tail $ [read $ fst x :: Int | x <- zip (map show [0..]) $ reverse $ intToBinary n, snd x == '0'])

intToBinary 0 = "0"
intToBinary n = reverse $ go n
    where
        go 0 = ""
        go n = let (q, r) = divMod n 2 in show r ++ go q

main = do
    print $ head $ tail $ [read $ fst x :: Int | x <- zip (map show [0..]) $ reverse $ intToBinary 1073741824, snd x == '0']
