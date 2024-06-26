import Sudoku
import Control.Exception
import System.Environment
import Control.Parallel.Strategies hiding (parMap)
import Data.Maybe

-- <<main
main :: IO ()
main = do
  [f] <- getArgs
  file <- readFile f

  let puzzles   = lines file
  evaluate ( length puzzles )
  let solutions = runEval (parMap solve puzzles)

  print (length (filter isJust solutions))
-- >>

-- <<parMap
parMap :: (a -> b) -> [a] -> Eval [b]
parMap f [] = return []
parMap f (a:as) = do
   b <- rpar (f a)
   bs <- parMap f as
   return (b:bs)
-- >>
