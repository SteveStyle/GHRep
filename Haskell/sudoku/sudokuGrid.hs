module SudokuGrid where
import Data.Char; import Data.List; import System.IO
import Util 

type Cell = [Int]; type Row = [Cell]; type Grid = [Row]

emptyCell = [1..9]                  :: Cell
emptyRow  = replicate 9 emptyCell   :: Row
emptyGrid = replicate 9 emptyRow    :: Grid

g2  = initGrid "    7  454     8  8 1r 8   2r   59  6 21   4r5  4    3  2     7   6   54" 
g3  = initGrid "  4 6  13 5r6 3     2 9   473        8   2 1   94r 7   59r  12    7" 

-- set the values of a cell in a given row
setCellRow :: Row->Int->Cell->Row
setCellRow r c v = (take c r) ++ v : (drop (c+1) r)

-- set the values of a cell in a grid, based on position, p, in 0..80
setCell :: Grid-> Int-> [Int]    ->Grid
setCell g p v = (take r g) ++ (setCellRow (g!!r) c v) : (drop (r+1) g)
    where r = p `div` 9; c = p `mod` 9

-- set a cell in a grid to a single digit
setDigit :: Grid->Int->Int->Grid
setDigit g p d = setCell g p [d]

{-- | setCellsSpacing takes a string of digits and filler characters.  r or R moves to the next row. -}
setCellsSpacing :: Grid->String->Grid
setCellsSpacing g "" = g
setCellsSpacing g xs = setCells' 0 xs g     where
  setCells' :: Int -> String -> Grid -> Grid
  setCells' p []     g                    = g
  setCells' p (v:xs) g | isDigit v        = setCells' (p+1) xs (setDigit g p (digitToInt v))
                       | toUpper v == 'R' = setCells' (((p `div` 9)+1)*9) xs g
                       | p == 80          = g
                       | otherwise        = setCells' (p+1) xs g

-- initialise a grid based on an input string
initGrid = setCellsSpacing emptyGrid :: String->Grid

-- convert a cell to a multi-digit integer for display purposes
showCellInt :: Cell -> Integer
showCellInt c = (strToInteger.concat.(map show)) c

printGrid :: Grid -> IO ()
printGrid g = sequence_ [ putStrLn (show (map showCellInt x)) | x <- g ]

showCellChar :: Cell -> Char
showCellChar [x] = intToDigit x
showCellChar c = ' '

printGridChar :: Grid -> IO ()
printGridChar g = sequence_ [ putStrLn (show (map showCellChar x)) | x <- g ]
                 