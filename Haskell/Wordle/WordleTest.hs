{-# LANGUAGE TransformListComp #-}
module WordleTest where

import WordleSolve
import WordleData
import WordleTestWord


import Control.Exception
import System.Environment
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




-- result calculated and stored in WordleData
mostSelective' :: [(String, Int)]
mostSelective' = take 100 [  (g, n)
                         | g <- guessListFull
                         , let ( _, n, _) = runEval $ rpar( head $ testWordtoTargets targetListFull g )
                         , then sortWith by n
                         ]
                         

responses :: String -> [CharResult] -> [String]
responses g rs = [ x 
                 | x <- targetListFull
                 , testWord g x  == rs
                 ]

resp x = responses "REAIS" $ s2cr x

sl x l = targetList2GuessList (resp x) l