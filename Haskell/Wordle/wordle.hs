{-# LANGUAGE TransformListComp #-}
module Wordle where
import WordleData
import WordleTestWord
import GHC.Exts

data ResponseNode = ResponseNode TestResult [Guess]
  deriving(Show)

data Guess =   Guess String [ResponseNode] Int Float -- guess, target list, max depth, mean depth
             | GuessLeaf String
  deriving (Show)


data Strategy = Strategy [String] 
  deriving (Show)
  
-- DepthLimit = 2



-- target list -> test word -> ( pattern, number of targets, list of targets )
testWordtoTargets :: [String] -> String -> [ ( TestResult, Int, [String] ) ]
testWordtoTargets xs y = [ ( the z, length x, x )
                         | x <- xs
                         , z <- [testWord y x ]
                         , then group by z using groupWith 
                         , then sortWith by (length x)                           
                         ]

-- target list -> depth -> guess -> response node list
guess2ResponseNodeList :: [String] -> Int -> String -> [ ResponseNode ]
guess2ResponseNodeList xs n y = [ ResponseNode (the z) (targetList2GuessList x n)
                                | x <- xs
                                , z <- [testWord y x ]
                                , then group by z using groupWith 
                                ]

-- target list -> depth -> list of guess nodes
targetList2GuessList :: [String] -> Int -> [Guess]
targetList2GuessList [t] n = [GuessLeaf t]
targetList2GuessList ts  n = [ Guess g (guess2ResponseNodeList ts (n+1) g) 0 0
                             | g <- guessList
                             ]