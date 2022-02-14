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



test1 = MaybeGuessList ( targetList2GuessList targetList 0
                       )


-- target list -> test word -> ( pattern, number of targets, list of targets )
testWordSel :: [String] -> String -> Int
testWordSel xs y = maximum [ length x
                           | x <- xs
                           , let z = testWord y x
                           , then group by z using groupWith 
                           ]


parMap' :: Int -> (a -> b) -> [a] -> [b]
parMap' _ _ [] = []
parMap' n f xs = concat $ parMap rpar (map f) (splitEvery n xs)


-- parMap' :: Int -> (a -> b) -> [a] -> [b]
-- parMap' _ _ [] = []
-- parMap' n f xs = concat $ runEval $ parMap'' n f (splitEvery n xs)

-- parMap'' :: Int -> (a -> b) -> [[a]] -> Eval [[b]]
-- parMap'' _ _ [] = return []
-- parMap'' n f (ys:xs') = ( do a <- rpar (map f ys)
                             -- bs <- parMap'' n f xs'
                             -- return (a:bs)
                        -- )

-- result calculated and stored in WordleData
mostSelective' :: [(Int, String)]
mostSelective' = take 100 $ sort $ filter (\(n,_) -> (n<200)) unsortedList
                 where
                 getMaxCount :: [String] -> String -> (Int,String)
                 getMaxCount ts g = force( ( (testWordSel ts g) ,g ) )
                 
--                 unsortedList = parMap' 100 (getMaxCount targetListFull) guessListFull


                 unsortedList = parMap rpar (getMaxCount targetListFull) guessListFull
                         
-- mostSelective' = take 100 $ [  (g, n)
                         -- | g <- guessListFull
                         -- , let ( _, n, _) = runEval $ do x <- rpar( force ( head $ testWordtoTargets targetListFull g ) )
                                                         -- return x
                         -- , then sortWith by n
                         -- ]
                         

responses :: String -> [CharResult] -> [String]
responses g rs = [ x 
                 | x <- targetListFull
                 , testWord g x  == rs
                 ]

resp x = responses "REAIS" $ s2cr x

sl x l = targetList2GuessList (resp x) l