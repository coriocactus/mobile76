fac 0 = 1
fac n = n * fac (n-1)

sneakySquare x y | x > y      = x^2 + y^2
                 | otherwise  = x^2 - y^2

main = do
  print (fac 5)
  print (sneakySquare 5 6)
  print (sneakySquare 6 5)
