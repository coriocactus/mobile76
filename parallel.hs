import Control.Parallel
import Criterion.Main

paraFib :: Int -> Int
paraFib n
  | n <= 20 = fib n
  | otherwise = par n1 (pseq n2 (n1 + n2))
      where n1 = paraFib (n-1)
            n2 = paraFib (n-2)

fib :: Int -> Int
fib n
  | n == 0 = 0
  | n == 1 = 1
  | otherwise = fib (n-1) + fib (n-2)

main :: IO ()
main = defaultMain
  [ bgroup "Fibonacci-30"
    [ bench "Sequential" $ nf fib 30
    , bench "Parallel"   $ nf paraFib 30
    ]
  ]
