{- 
Compiling for multi threaded and creating an event log:

ghc -O2 -threaded -rtsopts -eventlog Wordlerpar.hs

./Wordlerpar +RTS -N16 -l



Threadscope

I had to use use the linux apt tool, otherwise I had dependency issues, mainly on pkg-install

sudo apt install threadscope


In linux run with 

	threadscope Wordlerpar.eventlog



-}

import Control.Parallel
import Control.Parallel.Strategies
import Control.Exception
import Data.Time.Clock
import Text.Printf
import System.Environment
import Data.List
import Control.DeepSeq


import WordleData
import WordleTest
import WordleUtils
import WordleTestWord
import WordleSolve


                   
gs = guessListFull
ts = targetListFull

listChunks = splitEvery 100 gs


job3 = parMap'' rpar (maxCount ts) gs
job4 = parChunk rpar 100 (maxCount ts) gs

guesses = take 10 $ sort $ runEval( parChunk rpar 100 (maxCount ts) gs )

job5 = do 
         xs <- parMap'' rpar (\(n,g) -> ( do (n',g') <- bestPartners g; return ((n',n), (g,g')) ) ) guesses
         return $ take 100 $ sort $ concat $ xs
       where
         bestPartners g = map ( maxCount [ t | t <- ts, (testWord g t) == allMiss] )  [ g' | g' <- gs, (testWord g g') == allMiss] 

job6 = map (\(_,g) -> solveWithStart targetListFull g) guesses

job7 = solveWithStart targetListFull "STORE"


-- <<main
main = do
  t0 <- getCurrentTime
  printTimeSince t0
  r <- evaluate ( job7)
  printTimeSince t0
  print r
  printTimeSince t0
-- >>

printTimeSince t0 = do
  t1 <- getCurrentTime
  printf "time: %.2fs\n" (realToFrac (diffUTCTime t1 t0) :: Double)
