module SudokuSolve where
import Data.Char; import Data.List; import System.IO
import Util
import SudokuGrid

-- The grid is complete when each cell has precisely one candidate, so 81 candidates in total
isComplete :: Grid -> Bool
isComplete = (==81).length.concat.concat

-- return a list of completed grids
-- solveGrid :: Grid -> [Grid]
-- solveGrid g = case simGrid g of
                -- Nothing -> []
                -- Just g' -> if isComplete g' then [g'] 
                           -- else let f  = concat g'; fl = map length f; n = minimum ( filter (>1) fl )
                                    -- p  = (length.takeWhile (/= n)) fl
                                    -- c  = f!!p
                                -- in do c' <- c; solveGrid (setDigit g' p c')
                                
-- return a list of completed grids
solveSol :: Sol -> [Sol]
solveSol s = case simSol s of
                Nothing -> []
                Just (Sol g h) -> if isComplete g then [Sol g h] 
                                  else let f = concat g; fl = map length f; n = minimum ( filter (>1) fl )
                                           p = (length.takeWhile (/= n)) fl
                                           c = f!!p
                                       in do c' <- c; let g' = (setDigit g p c'); 
                                             solveSol (Sol g' ((Bifurcation (p+1) c' g g'):h))

showSolGrid :: Grid -> IO ()
showSolGrid = putStrLn.showSolList.solveSol.initSol

-- simplify a grid by simplifying each row, column and block
--simGrid :: Grid-> Maybe Grid
--simGrid g  = do g1 <- simRows g; g2 <- simCols g1; g3 <- simBlocks g2
                --if g3 == g then return g else simGrid g3

-- simplify a grid by simplifying each row, column and block
simSol :: Sol -> Maybe Sol
simSol s = do s1 <- simRows s; s2 <- simCols s1; s3 <- simBlocks s2
              if s3 == s then return s else simSol s3

-- simplify rows by removing impossible candidates considering the row only
simGroups :: GroupType -> Sol -> Maybe Sol
simGroups t (Sol g h) = do xs <- sequenceA (map (simGroup t) (zip [1..9] g))
                           let g' = map fst xs
                               h' = appendMHIs h (map snd xs)
                               h'' = if (g == g') then h' else ((UpdateGrid g g'):h')
                           Just (Sol g' h'')
                        where
                          appendMHIs :: [HistoryItem] -> [Maybe HistoryItem] -> [HistoryItem]
                          appendMHIs h'' [] = h''
                          appendMHIs h'' (Nothing:xs) = appendMHIs h'' xs
                          appendMHIs h'' ((Just x):xs) = appendMHIs (x:h'') xs
                        

-- simplify rows by removing impossible candidates considering the row only
simRows :: Sol -> Maybe Sol
simRows = simGroups Row

-- simplify columns by removing impossible candidates considering the column only
-- transposing the grid switches rows and columns
simCols :: Sol -> Maybe Sol
simCols (Sol g h) = do { (Sol g1 h1) <- simGroups Col (Sol (transpose g) h); return (Sol (transpose g1) h1) }

-- rearrange the grid to move blocks into rows
blocksToRows :: Grid -> Grid
blocksToRows = concat.map (map concat).map (split 3).transpose.map (split 3)

-- rearrange the grid to move rows into blocks
rowsToBlocks :: Grid -> Grid
rowsToBlocks = blocksToRows.blocksToRows

-- simplify blocks by removing impossible candidates considering the block only
-- move blocks into rows first, simplify the rows, and then switch the rows back to blocks
simBlocks :: Sol -> Maybe Sol
simBlocks (Sol g h) = do { (Sol g1 h1) <- simGroups Block (Sol (blocksToRows g) h); return (Sol (rowsToBlocks g1) h1)}

-- simplify a single row
-- construct all possible completed versions of the row, 
-- then combine into a single row with multiple candidates per cell
simGroup :: GroupType -> (Int,Group) -> Maybe ( Group, Maybe HistoryItem )
simGroup t ( i, (c1:c2:c3:c4:c5:c6:c7:c8:c9:[]) ) | length r == 0     = Nothing
                                                  | otherwise         = Just ( g2, makeHI)
  where r = [[x1,x2,x3,x4,x5,x6,x7,x8,x9] | 
            x1<-c1,
            x2<-c2,     x2 /= x1,
            x3<-c3,     x3 `notelem` [x1,x2],
            x4<-c4,     x4 `notelem` [x1,x2,x3],
            x5<-c5,     x5 `notelem` [x1,x2,x3,x4],
            x6<-c6,     x6 `notelem` [x1,x2,x3,x4,x5],
            x7<-c7,     x7 `notelem` [x1,x2,x3,x4,x5,x6],
            x8<-c8,     x8 `notelem` [x1,x2,x3,x4,x5,x6,x7],
            x9<-c9,     x9 `notelem` [x1,x2,x3,x4,x5,x6,x7,x8]
            ]
        g1 = (c1:c2:c3:c4:c5:c6:c7:c8:c9:[])
        g2 = ((map sortdedupe).transpose) r
        makeHI :: Maybe HistoryItem
        makeHI = if g1 == g2 then Nothing
                 else Just (UpdateGroup t i g1 g2)
        
