double x = x + x
quadruple x = double (double x)
factorial n = product [1..n]
average ns = sum ns `div` length ns
plus2 n = n + 2

sproduct [] = 1
sproduct (h:ts) = h* sproduct ts

revsort [] = []
revsort (h:ts) = revsort larger ++ [h] ++ revsort smaller
                 where
                   smaller = [a | a <- ts, a < h]
                   larger = [a | a<- ts, a > h]
                   
n = a `div` length xs
  where
    a = 10
    xs = [1,2,3,4,5]
