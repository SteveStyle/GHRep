import Util

squares :: [Int]
squares = squares' 0

squares' :: Int -> [Int]
squares' n = (n * n) : squares' (n+1)

squaresNdts n = takeWhile (\x -> x < (10^n) ) (filter (\x -> x >= (10^(n-1))) squares)
squaresNdtsAllSq n = filter checkSquareDigits (squaresNdts n)

-- digits :: base -> number -> list of digits
digits :: Int -> Int -> [Int]
digits b n = digits' b n []


-- digits' :: base -> number -> partial list -> complete list
digits' :: Int -> Int -> [Int] -> [Int]
digits' b 0 xs = xs
digits' b n xs = digits' b (n `div` b) ((n `mod` b):xs)

checkSquareDigits :: Int -> Bool
checkSquareDigits n = all isSquareDigit (digits 10 n)

-- isSquareDigit :: digit -> Bool   true if digit is 0,1,4,5,6 or 9
isSquareDigit :: Int -> Bool
isSquareDigit d = (d `elem` [0,1,4,5,6,9])

isQR :: Int -> Int -> Bool
isQR 0 n = isQR ((length.digits 10) n) n
isQR d n = n' `elem` residues
  where
    n' = n `mod` (10^d)
    residues = sortdedupe [i^2 `mod` (10^d) | i <- [0..(10^d)//2] ]


--solve :: [[Int]]
--solve = across
--  where
    


-- test :: [[Int]] -> String
-- test across = assert (length xs == 4) res
  -- where
    -- down = transpose across
    -- res = assert (all isSquare across) (show across)