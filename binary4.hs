solution n m =
  2 ^ (31 - last [fst x | x <- zip [0..]
    $ zip (pad32 $ intToBinary n) (pad32 $ intToBinary m)
    , let y = snd x, fst y /= snd y])

intToBinary 0 = "0"
intToBinary n = reverse $ go n
    where 
      go 0 = ""
      go n = let (q, r) = divMod n 2 in show r ++ go q

pad x y = replicate (x - length y) '0' ++ y
pad32 = pad 32

main = do
  print $ solution 11 13
