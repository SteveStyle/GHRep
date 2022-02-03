{-# LANGUAGE TransformListComp #-}
module Wordle where
import WordleTestWord
import GHC.Exts

targetList :: [String]
targetList = ["ALLOY", "PILOT", "QUERY", "ENTRY", "ALLAY", "STING",
              "PLUCK", "QUEEN", "WRECK", "CRANK", "SHEEP", "HIRED", 
              "WIRED", "FRIED", "SORRY", "HEARD", "PUSHY", "LIMBO",
              "TARED", "TARES", "LUCKY", "FLUKE", "FLAKE", "CLICK",
              "DADDY", "MUMMY", "MUSHY", "SWELL", "SWINE", "COULD",
              "TEARY", "SOLID", "CLUCK", "DIDDY", "WOUND", "WOULD",
              "SAUCE", "SALSA", "GREEN", "DRAKE", "FIRED", "SPEAK",
              "TIMED", "VIOLA", "RAISE", "PRISE", "PAUSE", "PARTY"
             ]

guessList :: [String]
guessList = targetList

data Guess = Guess String [String] Int Float -- guess, target list, max depth, mean depth

data Strategy = Strategy [String] 
  deriving (Show)
  


-- target list -> test word -> ( pattern, number of targets, list of targets )
testWordtoTargets :: [String] -> String -> [ ( TestResult, Int, [String] ) ]
testWordtoTargets xs y = [ ( the z, length x, x )
                         | x <- xs
                         , z <- [testWord y x ]
                         , then group by z using groupWith 
                         , then sortWith by (length x)                           
                         ]


