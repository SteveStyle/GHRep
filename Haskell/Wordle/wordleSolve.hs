{-# LANGUAGE TransformListComp #-}
module WordleSolve where
import WordleData
import WordleTestWord
import GHC.Exts
import Data.Maybe

type Level = Int

showList' :: Show a => [a] -> String
showList' xs = foldl (\x y -> (x ++ (show y))) "" xs

data ResponseNode = ResponseNode Level TestResult [Guess]
  --deriving(Show)
  
instance Show ResponseNode where
--  show (ResponseNode l tr [GuessLeaf g]) = "\n" ++ (replicate (4*l + 2) ' ') ++ "RN " ++ (concat $ map show tr) ++ ' ' : g
  show (ResponseNode l tr [GuessLeaf g]) = "\n" ++ (replicate (4*l + 2) ' ') ++ (concat $ map show tr) ++ ' ' : g
  show (ResponseNode l tr gs) = "\n" ++ (replicate (4*l + 2) ' ') ++ (concat $ map show tr) ++ ' ' : showList' gs

data Guess =   Guess Level String [ResponseNode] -- Int Float -- guess, target list, max depth, mean depth
             | GuessLeaf String
--  deriving (Show)

instance Show Guess where
--  show (Guess l g rs) = "\n" ++ (replicate (4*l) ' ') ++ "G " ++ g ++ ' ':(showList' rs)
  show (Guess l g rs) = "\n" ++ (replicate (4*l) ' ') ++ g ++ ' ':(showList' rs)
  show (GuessLeaf g) = "L " ++ g


data Strategy = Strategy [String] 
  deriving (Show)
  
depthLimit = 3



-- target list -> test word -> ( pattern, number of targets, list of targets )
testWordtoTargets :: [String] -> String -> [ ( TestResult, Int, [String] ) ]
testWordtoTargets xs y = [ ( the z, length x, x )
                         | x <- xs
                         , z <- [testWord y x ]
                         , then group by z using groupWith 
                         , then sortWith by (length x)                           
                         ]

-- target list -> depth -> guess -> response node list
guess2ResponseNodeList :: [String] -> Int -> String -> Maybe [ ResponseNode ]
guess2ResponseNodeList ts n g = sequenceA [ do gs <- (targetList2GuessList t (n+1) ); return (ResponseNode n (the r) gs)
                                          | t <- ts
                                          , let r = testWord g t
--                                          , z <- [testWord y x ]
                                          , then group by r using groupWith 
                                          ]

-- target list -> depth -> list of guess nodes
targetList2GuessList :: [String] -> Int -> Maybe [Guess]
targetList2GuessList []  _ = Nothing
targetList2GuessList [t] _ = Just [GuessLeaf t]
targetList2GuessList ts  n | n == depthLimit = Nothing
                           | otherwise       = let gs = catMaybes (do g <- ts;  -- guessList;  -- try restricting to the current target list
                                                                      return (do rs <- (guess2ResponseNodeList ts (n) g); return (Guess n g rs) )
                                                                  )
                                               in if null gs then Nothing else Just [head gs] -- gs

                           -- | otherwise       = Just (do g <- guessList;
                                                        -- let rs = (guess2ResponseNodeList ts (n+1) g);
                                                        -- if null rs then [] else return (Guess g rs 0 0)
                                                    -- )
                                                  
                           
--                           | otherwise       = Just [ Guess g (guess2ResponseNodeList ts (n+1) g) 0 0
--                                                    | g <- guessList
--                                                    ]
