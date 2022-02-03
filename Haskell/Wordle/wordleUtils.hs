module WordleUtils where

-- removes just the first occurence of an element from a list, or none if not present
removeElem :: Eq a => a -> [a] -> [a]
removeElem x [] = []
removeElem x (y:ys) | (x == y) = ys
                    | otherwise = y : (removeElem x ys)

