module WordleTest where

import WordleSolve
import WordleData

newtype MaybeGuessList = MaybeGuessList (Maybe [Guess])

instance Show MaybeGuessList where
  show (MaybeGuessList Nothing) = "Nothing"
  show (MaybeGuessList (Just gs)) = show $ head $ gs

showMaybeGuessList :: Maybe [Guess] -> String
showMaybeGuessList Nothing   = "Nothing"
showMaybeGuessList (Just gs) = show $ head $ gs

test1 = MaybeGuessList ( targetList2GuessList targetList 0
                       )

