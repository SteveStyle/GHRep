module SudokuSolve where
import Data.Char; import Data.List; import System.IO
import Util
import SudokuGrid

isComplete :: Grid -> Bool
isComplete = (==81).length.concat.concat

solveGrid :: Grid -> [Grid]
solveGrid g = case simGrid g of
                Nothing -> []
                Just g' -> if isComplete g' then [g'] 
                           else let f  = concat g'; fl = map length f; n = minimum ( filter (>1) fl )
                                    p  = (length.takeWhile (/= n)) fl
                                    c  = f!!p
                                in do c' <- c; solveGrid (setDigit g' p c')

simGrid :: Grid-> Maybe Grid
simGrid g  = do g1 <- simRows g; g2 <- simCols g1; g3 <- simBlocks g2
                if g3 == g then return g else simGrid g3

simRows :: Grid -> Maybe Grid
simRows = sequenceA.map simRow

simCols :: Grid -> Maybe Grid
simCols g = do { g1 <- (simRows.transpose) g; return (transpose g1) }

blocksToRows :: Grid -> Grid
blocksToRows = concat.map (map concat).map (split 3).transpose.map (split 3)

rowsToBlocks :: Grid -> Grid
rowsToBlocks = blocksToRows.blocksToRows

simBlocks :: Grid -> Maybe Grid
simBlocks g = do { g1 <- (simRows.blocksToRows) g; return (rowsToBlocks g1) }

simRow :: Row -> Maybe Row
simRow (c1:c2:c3:c4:c5:c6:c7:c8:c9:[])   | length r == 0     = Nothing
                                         | otherwise         = Just (((map sortdedupe).transpose) r)
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
