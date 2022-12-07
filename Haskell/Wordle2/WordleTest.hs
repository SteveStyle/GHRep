{-# LANGUAGE TransformListComp #-}
module WordleTest where

import WordleSolve
import WordleData
import WordleTestWord
import WordleUtils

import Data.List
import Control.Exception
import System.Environment
import Control.DeepSeq
import Control.Parallel.Strategies

import GHC.Exts


newtype MaybeGuessList = MaybeGuessList (Maybe [Guess])

instance Show MaybeGuessList where
  show (MaybeGuessList Nothing) = "Nothing"
  show (MaybeGuessList (Just gs)) = show $ head $ gs
  
showMaybeGuessList :: Maybe [Guess] -> String
showMaybeGuessList Nothing   = "Nothing"
showMaybeGuessList (Just gs) = show $ head $ gs



test1' = MaybeGuessList ( targetList2GuessList targetList 0
                       )



parMap' :: Int -> (a -> b) -> [a] -> [b]
parMap' _ _ [] = []
parMap' n f xs = concat $ parMap rpar (map f) (splitEvery n xs)
--parMap' n f xs = concat $ parMap rpar (force $ map f) (splitEvery n xs)


-- parMap' :: Int -> (a -> b) -> [a] -> [b]
-- parMap' _ _ [] = []
-- parMap' n f xs = concat $ runEval $ parMap'' n f (splitEvery n xs)

-- parMap'' :: Int -> (a -> b) -> [[a]] -> Eval [[b]]
-- parMap'' _ _ [] = return []
-- parMap'' n f (ys:xs') = ( do a <- rpar (map f ys)
                             -- bs <- parMap'' n f xs'
                             -- return (a:bs)
                        -- )



mostSelective' = mostSelective'' targetListFull guessListFull

-- result calculated and stored in WordleData
mostSelective'' :: [String] -> [String] -> [(Int, String)]
mostSelective'' ts gs = take 100 $ sort $ filter (\(n,_) -> (n<200)) unsortedList
                 where

--                 getMaxCount ts g = force( ( (testWordSel ts g) ,g ) )
                 
                 unsortedList = parMap' 10 (maxCount ts) gs


--                 unsortedList = parMap rpar (getMaxCount targetListFull) guessListFull
                         
-- mostSelective' = take 100 $ [  (g, n)
                         -- | g <- guessListFull
                         -- , let ( _, n, _) = runEval $ do x <- rpar( force ( head $ testWordtoTargets targetListFull g ) )
                                                         -- return x
                         -- , then sortWith by n
                         -- ]
                         


resp x = responses "REAIS" $ s2cr x

sl x l = targetList2GuessList (resp x) l