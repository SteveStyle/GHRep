{-# LANGUAGE ScopedTypeVariables  #-}
module WordleUtils ( removeElem, showList', splitEvery, parMap'', parChunk, forceEval, foldMin ) where

import Control.Parallel
import Control.Parallel.Strategies
import Data.List
import Control.DeepSeq

-- removes just the first occurence of an element from a list, or none if not present
removeElem :: Eq a => a -> [a] -> [a]
removeElem x [] = []
removeElem x (y:ys) | (x == y) = ys
                    | otherwise = y : (removeElem x ys)

-- concatenates the contents of a list, as opposed to the default bracketed list format
showList' :: Show a => [a] -> String
showList' xs = foldl (\x y -> (x ++ (show y))) "" xs

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = as : splitEvery n bs 
  where (as,bs) = splitAt n xs




-- <<parMap''
parMap'' :: NFData b => (b -> Eval b) -> (a ->b) -> [a] -> Eval [b]
parMap'' _ _ [] = return []
parMap'' s f (x:xs) = do x' <- ( s $ force $ f x ) -- `using` s)
                         xs' <- (parMap'' s f xs)
                         return $ x' : xs'
-- >>

-- <<parChunk
parChunk :: NFData b => ([b] -> Eval [b]) -> Int -> (a ->b) -> [a] -> Eval [b]
parChunk _ _ _ [] = return []
parChunk s n f (x:xs) = let (as,bs) = splitAt n xs in
                        do as' <- ( s $ force ( (map f as ) ) )
                           bs' <- ( parChunk s n f bs )
                           return $ as' ++ bs'
-- >>



-- <<forceEval
forceEval :: NFData a => Eval a -> a
forceEval a = runEval ( do a' <- a
                           return $ force a'
                      )
-- >>

-- <<foldMin  takes a list of seed values and returns the first value with the minimal weight, where 1 is the smallest weight possible
foldMin :: forall a b. (a -> Maybe b) -> (b -> Int) -> Maybe b -> [a] -> Maybe b
foldMin _ _ m [] = m
foldMin f g mb (a:as) = foldMin' mb (f a)
                        where
                          foldMin' :: Maybe b -> Maybe b -> Maybe b
                          foldMin' mb Nothing = foldMin f g mb as
                          foldMin' Nothing (Just b') | (g b') == 1 = Just b'
                                                     | otherwise = foldMin f g (Just b') as
                          foldMin' (Just b) (Just b') | l' == 1 = Just b'
                                                      | l <= l' = foldMin f g (Just b) as
                                                      | otherwise = foldMin f g (Just b') as
                                                      where l = g b
                                                            l' = g b'

-- >>


   
