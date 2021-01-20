module Util where

import Data.Char
import Data.List
import System.IO

(%) = mod
(//) = div

notelem :: Eq a => a -> [a] -> Bool
notelem x xs = not (x `elem` xs)

--(elm) :: Eq a => a -> [a] -> Bool
--(elm) x ys = elem x ys

-- (notelm) :: Eq a => a -> [a] -> Bool
-- (notelm) x xs = not (x `elem` xs)


{-- | sortdedupe' xs ys = sortdepute xs ++ ys -}
sortdedupe' :: Ord a => [a] -> [a] -> [a]
sortdedupe' [] ys = ys
sortdedupe' (x:xs) ys = sortdedupe' lower (x : sortdedupe' upper ys) where
  upper = [y | y <- xs, y > x]
  lower = [y | y <- xs, y < x]

sortdedupe xs = sortdedupe' xs []


split :: Int->[a]->[[a]]
split n [] = []
split n xs = (take n xs) : (split n (drop n xs))

repfun :: (a->a)->Int->a->a
repfun f 0 x        = x
repfun f n x | n>0  = repfun f (n-1) (f x)

strToInteger :: String -> Integer
strToInteger s = read s :: Integer




type TestCell = (Int,Int)
type TestRow = [TestCell]
type TestGrid = [TestRow]
 
testGrid ::TestGrid
testGrid = split 9 [ (r,c) | r<-[1..9], c<-[1..9] ]

testBlocks :: TestGrid
testBlocks = testBlocksToRows testGrid

printAnyGrid :: Show a => [a] -> IO ()
printAnyGrid g = sequence_ [ putStrLn (show x) | x <- g ]

testBlocksToRows :: TestGrid -> TestGrid
testBlocksToRows g = (concat.map (map concat).map (split 3).transpose.map (split 3)) g

testRowsToBlocks :: TestGrid -> TestGrid
testRowsToBlocks g = testBlocksToRows (testBlocksToRows g)

testCheck :: Bool
testCheck = testGrid == (testRowsToBlocks (testBlocksToRows testGrid))