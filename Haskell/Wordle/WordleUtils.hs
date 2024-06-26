module WordleUtils ( removeElem, showList' ) where

-- removes just the first occurence of an element from a list, or none if not present
removeElem :: Eq a => a -> [a] -> [a]
removeElem x [] = []
removeElem x (y:ys) | (x == y) = ys
                    | otherwise = y : (removeElem x ys)

-- concatenates the contents of a list, as opposed to the default bracketed list format
showList' :: Show a => [a] -> String
showList' xs = foldl (\x y -> (x ++ (show y))) "" xs





