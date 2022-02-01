module SudokuGrid where
import Data.Char; import Data.List; import System.IO
import Util 

type Cell = [Int]; type Group = [Cell]; type Grid = [Group] -- a grid is a list of rows
data GroupType = Row | Col | Block
     deriving (Eq, Show)
data HistoryItem =   UpdateGrid Grid Grid  -- previous grid, updated grid
                   | UpdateGroup GroupType Int Group Group -- group type, group index, previous group, new group
                   | Bifurcation Int Int Grid Grid -- Cell no, cell value, previous grid, updated grid
     deriving (Eq, Show)
data Sol = Sol Grid [HistoryItem]
     deriving (Eq, Show)

emptyCell = [1..9]                  :: Cell
emptyRow  = replicate 9 emptyCell   :: Group
emptyGrid = replicate 9 emptyRow    :: Grid

g2 = initGrid "    7  454     8  8 1r 8   2r   59  6 21   4r5  4    3  2     7   6   54" 
g3 = initGrid "  4 6  13 5r6 3     2 9   473        8   2 1   94r 7   59r  12    7" 
g4 = initGrid "   3 9r 2  8  5r  1   7r4   2   3   1 3 7r1   6   8  7   3r 8  5  4r   9 2"
g5 = initGrid " 7  5  1r    286r2r     6r53      7 8  9  4r6      81  53r     937"
g6 = initGrid "    1   9 5  2r   8 9r246   783 7  4  9r  8   5r 1 574 2r 35   64r4       5"

-- set the values of a cell in a given row
setCellGroup :: Group->Int->Cell->Group
setCellGroup r c v = (take c r) ++ v : (drop (c+1) r)

-- set the values of a cell in a grid, based on position, p, in 0..80
setCell :: Grid-> Int-> [Int]    ->Grid
setCell g p v = (take r g) ++ (setCellGroup (g!!r) c v) : (drop (r+1) g)
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

initSol :: Grid -> Sol
initSol g = Sol g []

-- convert a cell to a multi-digit integer for display purposes
showCellInt :: Cell -> Integer
showCellInt c = (strToInteger.concat.(map show)) c

printGrid :: Grid -> IO ()
printGrid g = sequence_ [ putStrLn (show (map showCellInt x)) | x <- g ]

printGrids :: [Grid] -> IO ()
printGrids [] = do print "finished"
printGrids (x:xs) = do printGridChar x; printGrids xs

showCellChar :: Cell -> Char
showCellChar [x] = intToDigit x
showCellChar c = ' '

--printGridChar :: Grid -> IO ()
--printGridChar g = sequence_ [ putStrLn (show (map showCellChar x)) | x <- g ]

printGridChar :: Grid -> IO ()
printGridChar g = do  sequence_ [putStrLn (map showCellChar x) | x <- g ]

printSol :: Sol -> IO ()
printSol (Sol g h) = do printGridChar g; print h

printSols ::[Sol] -> IO ()
printSols [] = do print "finished"
printSols (x:xs) = do printSol x; printSols xs

showGrid :: Grid -> String
showGrid g = concat ( iEA (lineBreak2:(concat (iEA (split 3 (map showRow g) ) [lineBreak2]))) "\n"  )
--showGrid g = concat ( iEA (concat (iEB (split 6 (iEA (map showRow g) lineBreak1)) [lineBreak2])) "\n"  )
-- showGrid g = concat ( rowListBreaks3 )
   where 
     lineBreak1 =  replicate 22 ' '
     lineBreak2 =  concat (replicate 22 "-")
    
showRow :: Group -> String
showRow r = '|':(concat ( iEA (split 6 (iEA (map showCellChar r) ' ')) ['|']) )
--showRow r = concat ( iEB (split 6 (iEA (map showCellChar r) '|')) ['|']) 

showCellCandidates :: Cell -> String
showCellCandidates c = showCellCandidates' c 1
  where  
    showCellCandidates' :: Cell -> Int -> String
    showCellCandidates' c 10 = ""
    showCellCandidates' c n = let ch = if n `elem` c then intToDigit n else ' '
                              in ch:showCellCandidates' c (n+1)
                              
showRowCandidates :: Group -> String
showRowCandidates r = let r' = concat (iEA (map showCellCandidates r) ",")
                          l = length r'
                      in take (l-1) r'

showGridCandidates:: Grid -> String
showGridCandidates g = concat (iEA (map showRowCandidates g) "\n")

showHistoryItem :: HistoryItem -> String
showHistoryItem (UpdateGroup t i r1 r2) = (show t) ++ " " ++ (show i) ++ " updated\n" ++ (showRowCandidates r1) ++ "\nchanged to\n" ++ (showRowCandidates r2) ++ "\n"
showHistoryItem (UpdateGrid g1 g2) = "Grid changed from:\n" ++ (showGridCandidates g1) ++ "\nto:\n" ++ (showGridCandidates g2) ++ "\n"
showHistoryItem (Bifurcation cn cv g1 g2) = "Bifurcation on cell " ++ (show cn) ++ " putting equal to " ++ (show cv) ++ "\nchanging grid from " ++ (showGridCandidates g1) ++ "\nto:\n" ++ (showGridCandidates g2) ++ "\n"

showHistory :: [HistoryItem] -> String
showHistory [] = ""
showHistory (h:hs) = (showHistory hs) ++(showHistoryItem h) 

showSolution :: Sol -> String
showSolution (Sol g h) = (showGridCandidates g) ++ (showHistory h)

showSolList :: [Sol] -> String
showSolList s = concat (map showSolution s)



-- interleaveElemAfter adds a specified element after each element in a list
iEA :: [a] -> a -> [a]
iEA [] y = []
iEA (x:xs) y = x:y:(iEA xs y)

-- interleaveElemBefore adds a specified element after each element in a list
iEB :: [a] -> a -> [a]
iEB [] y = []
iEB (x:xs) y = y:x:(iEB xs y)
                 