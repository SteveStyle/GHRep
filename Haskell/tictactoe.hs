import Data.Char
import Data.List
import System.IO

-- count the number of occurences of x in xs
cnt :: Eq a => a -> [a] -> Int
cnt x xs = length (filter (==x) xs)



size :: Int
size = 3

type Grid = [[Player]]  -- defines a synomym for [[Player]]
data Player = O|B|X     -- defines the Player type, B means blank and is included to simplify the definition of Grid
  deriving (Eq,Ord,Show)
players = [O,B,X]       -- it is useful to have a list of all possible players

next :: Player -> Player
next O = X
next X = O
next B = B

empty :: Grid
empty = replicate size (replicate size B)

test1 :: Grid
test1 = [[O,B,X],[B,O,X],[X,X,X]]

full :: Grid -> Bool
full g = all (/=B) (concat g)

turn :: Grid -> Player
turn g = if ocnt <= xcnt  then O else X
  where ocnt = cnt O (concat g)
        xcnt = cnt X (concat g)

diag :: Grid -> [Player]
diag g = [ g !! n !! n | n <- [0..size-1] ]

wins :: Player -> Grid -> Bool
wins p g = any (==3) [cnt p x| x <- h]
  where h = (diag g): (diag (reverse g)): g ++ (transpose g)
  
won :: Grid -> Bool
won g = wins O g || wins X g

putGrid :: Grid -> IO ()
putGrid = 
  putStrLn.unlines.concat.interleave bar.map showRow
  where 
    bar = [replicate ((size*4)-1) '-']

showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer
  where
    beside = foldr1 (zipWith (++))
    bar    = replicate 3 "|"

showPlayer :: Player -> [String]
showPlayer O = ["   "," O ","   "]
showPlayer B = ["   "," B ","   "]
showPlayer X = ["   "," X ","   "]

interleave :: a -> [a] -> [a]
interleave x [] = []
interleave x [y] = [y]
interleave x (y:ys) = y : x : interleave x ys

valid :: Grid -> Int -> Bool
valid g n = 0 <= n && n < size^2 && concat g !! n == B

move :: Grid -> Int -> Player -> [Grid]
move g i p = if valid g i then [chop size (xs++[p]++ys)] else []
  where (xs,B:ys) = splitAt i (concat g)
  
chop :: Int -> [a] -> [[a]]
chop _ [] = []
chop i xs = take i xs : chop i (drop i xs)