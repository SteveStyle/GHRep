import Data.Char
import Data.List
import System.IO
import System.Random


rnd = 1

-- SquidBridge Pairs -> Players -> Deaths
squidBridge :: Int -> Int -> Int
squidBridge 0 y = 0
squidBridge x 0 = 0
squidBridge x y = r + SquidBridge (x-1) (x-r)
    where r = rnd
