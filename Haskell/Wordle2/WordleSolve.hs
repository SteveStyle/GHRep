{-# LANGUAGE TransformListComp #-}
module WordleSolve where

import WordleData
import WordleTestWord
import WordleUtils

import Control.Parallel
import Control.Parallel.Strategies
import Control.DeepSeq
import Data.List

import GHC.Exts
import Data.Maybe

type Level = Int

data ResponseNode = ResponseNode Level TestResult [Guess]
  --deriving(Show)
  
instance Show ResponseNode where
--  show (ResponseNode l tr [GuessLeaf g]) = "\n" ++ (replicate (4*l + 2) ' ') ++ "RN " ++ (concat $ map show tr) ++ ' ' : g
  show (ResponseNode l tr [GuessLeaf g]) = "\n" ++ (replicate (4*l + 2) ' ') ++ (concat $ map show tr) ++ ' ' : g
  show (ResponseNode l tr gs) = "\n" ++ (replicate (4*l + 2) ' ') ++ (concat $ map show tr) ++ ' ' : showList' gs

instance NFData ResponseNode where
  rnf (ResponseNode l tr gs) = rnf l `seq` rnf tr `seq` rnf gs `seq` ()

data Guess =   Guess Level String [ResponseNode] -- Int Float -- guess, target list, max depth, mean depth
             | GuessLeaf String
--  deriving (Show)

instance Show Guess where
--  show (Guess l g rs) = "\n" ++ (replicate (4*l) ' ') ++ "G " ++ g ++ ' ':(showList' rs)
  show (Guess l g rs) = "\n" ++ (replicate (4*l) ' ') ++ g ++ ' ':(showList' rs)
  show (GuessLeaf g) = "L " ++ g
  
instance NFData Guess where
  rnf (Guess l ts rns) = rnf l `seq` rnf ts `seq` rnf ts `seq` rnf rns `seq` ()
  rnf (GuessLeaf x) = rnf x `seq` ()


data Strategy = Strategy [String] 
  deriving (Show)
  
depthLimit = 4



-- target list -> test word -> ( pattern, number of targets, list of targets )
testWordtoTargets :: [String] -> String -> [ ( TestResult, Int, [String] ) ]
testWordtoTargets xs y = [ ( the z, length x, x )
                         | x <- xs
                         , z <- [testWord y x ]
                         , then group by z using groupWith 
                         , then sortWith by (- length x)                           
                         ]
                         
                         
-- solver which just returns the best guess at each stage
-- take a target list and return the best guess
solveWordle :: [String] -> Maybe Guess
solveWordle [] = Nothing
solveWordle [t] = Just (GuessLeaf t)
--solveWordle ts = foldMin makeGuess (guessDepth depthLimit) Nothing (ts ++ guessList)




-- target list -> depth -> guess -> response node list
guess2ResponseNodeList :: [String] -> Level -> String -> Maybe [ ResponseNode ]
guess2ResponseNodeList ts n g = sequenceA [ do gs <- (targetList2GuessList t (n+1) ); return (ResponseNode n (the r) gs)
                                          | t <- ts
                                          , let r = testWord g t
--                                          , z <- [testWord y x ]
                                          , then group by r using groupWith 
                                          ]

-- target list -> depth -> list of guess nodes
targetList2GuessList :: [String] -> Level -> Maybe [Guess]
targetList2GuessList []  _ = Nothing
targetList2GuessList [t] _ = Just [GuessLeaf t]
targetList2GuessList ts  n | n == depthLimit = Nothing
                           | otherwise       = firstGuess (ts ++ guessList)
                                               where
                                                 firstGuess :: [String] -> Maybe [Guess]
                                                 firstGuess [] = Nothing
                                                 firstGuess (g:gs) = firstGuess' g gs (guess2ResponseNodeList ts n g) 
                                                 
                                                 firstGuess' :: String -> [String] -> Maybe [ResponseNode] -> Maybe[Guess]
                                                 firstGuess' _ gs Nothing = firstGuess gs
                                                 firstGuess' g _ (Just rns) = Just [Guess n g rns]
                                                                     

                           -- | otherwise       = let gs = catMaybes (do g <- (ts ++ guessList);  -- guessList;  -- try restricting to the current target list
                                                                      -- return (do rs <- (guess2ResponseNodeList ts n g); return (Guess n g rs) )
                                                                  -- )
                                               -- in if null gs then Nothing else Just [head gs] -- gs



                           -- | otherwise       = Just (do g <- guessList;
                                                        -- let rs = (guess2ResponseNodeList ts (n+1) g);
                                                        -- if null rs then [] else return (Guess g rs 0 0)
                                                    -- )
                                                  
                           
--                           | otherwise       = Just [ Guess g (guess2ResponseNodeList ts (n+1) g) 0 0
--                                                    | g <- guessList
--                                                    ]

-- target list -> 
solveWithStart :: [String] -> String -> Maybe Guess
solveWithStart ts g = do rs <- sequenceA $ runEval responseNodes
                         return (Guess 0 g rs)
                      where
                        responseNodes = parMap'' rpar f responses
                        f ( r, ts ) = do gns <- (targetList2GuessList ts 1); return (ResponseNode 0 r gns)
                        responses = [ ( the r, t )
                                    | t <- ts
                                    , let r = testWord g t
                                    , then group by r using groupWith 
                                    ]

-- guess, result -> strategy
solveFrom2ndGuess :: String -> String -> Level -> Maybe [Guess]
solveFrom2ndGuess g crs l = targetList2GuessList (responses g $ s2cr crs) l

solveFromGuesses :: [(String,String)] -> Level -> Maybe [Guess]
solveFromGuesses gs l = targetList2GuessList (responsesx targetListFull gs) l

responses :: String -> [CharResult] -> [String]
responses g rs = sort [ x 
                      | x <- targetListFull
                      , testWord g x  == rs
                      ]
                      
-- target list, list of [guess, result] -> reduced target list
responsesx :: [String] -> [(String,String)] -> [String]
responsesx ts [] = ts
responsesx ts ((g,crs):xs) = responsesx (filter (\x -> (testWord g x == (s2cr crs))) ts) xs 