import Data.Char
import Data.List
import System.IO
import Primes

-- Problem from a tweet by Alex...

-- Provide numbers for two dice so that the sum is always prime.
-- It must be that one die has odd numbers and the other has even numbers.
-- More generally it cannot be that all the possible mod n values are present on a single die.

-- The approach is greedy.  We will build up the numbers on each die alternately starting with the odd die.  
-- We always choose the lowest number which does not contain all the mod n values, and which makes a prime when summed with the other die.

{-- | sorts and deduplicaes a list -}
sortdedupe :: Ord a => [a] -> [a]
sortdedupe [] = []
sortdedupe (x:xs) = sortdedupe [y | y <- xs, y < x] ++ [x] ++ sortdedupe [y | y <- xs, y > x]

{-- | sortdedupe' xs ys = sortdepute xs ++ ys -}
sortdedupe' :: Ord a => [a] -> [a] -> [a]
sortdedupe' [] ys = ys
sortdedupe' (x:xs) ys = sortdedupe' lower (x : sortdedupe' upper ys) where
  upper = [y | y <- xs, y > x]
  lower = [y | y <- xs, y < x]

sortdedupe2 xs = sortdedupe' xs []

{-- | False if the list contains all of the mod values for any number from 2 up to n -}
isValidMod :: Integer -> [Integer] -> Bool
isValidMod n xs | n <= 1    = True
                | otherwise =  (((<n).toInteger.length.sortdedupe.(map (\x -> x `mod` n))) xs) && isValidMod (n-1) xs

{-- | False if the list contains all of the mod values for any number 2 and above -}
isValidModL :: [Integer] -> Bool
isValidModL xs = isValidMod ((toInteger.length) xs) xs

isValidPrime :: [Integer] -> Integer -> Bool
isValidPrime xs n = all (isPrimeP.(+n)) xs

{-- | that die, this die, candidate for this die -}
isValid :: [Integer] -> [Integer] -> Integer -> Bool
isValid ys xs x = isValidModL (x:xs) && isValidPrime ys x && all (x>) xs && not (elem x (xs ++ ys))

isValidPrimePair :: [[Integer]] -> Bool
isValidPrimePair [xs,ys] = all (isValidPrime xs) ys

{-- | that die, this die -> next integer for this -}
nextDieValue :: [Integer] -> [Integer] -> Integer
nextDieValue ys xs = head (filter (isValid ys xs) [1..])

{-- | extend Dies adds one more face to each die, expects [die1, die2] -}
extendDice :: [[Integer]] -> [[Integer]]
extendDice [xs,ys] = fixDice [xs',ys']
  where
    xs' = xs ++ [nextDieValue ys xs]
    ys' = ys ++ [nextDieValue xs' ys]
    
{-- | rep n f x applies f to x n times, giving f^n x -}
rep :: Integer -> (a->a) -> a -> a
rep 0 f x = x
rep n f x = rep (n-1) f (f x)

{-- | fixDice [die1, die2] extends one die until each has the same number of faces -}
fixDice :: [[Integer]] -> [[Integer]]
fixDice [xs,ys] | length xs == length ys    = [xs,ys]
                | length xs < length ys     = fixDice [xs ++ [nextDieValue ys xs],ys]
                | length xs > length ys     = fixDice [xs, ys ++ [nextDieValue xs ys]]


{-- | findSolution n [xs,xy], finds a solution with n faces for two dice -}
findSolution :: Integer -> [[Integer]] -> [[Integer]]
findSolution n [xs,ys] = rep n' extendDice [xs',ys']
  where [xs',ys'] = fixDice [xs,ys]
        n' = n - ((toInteger.length) xs')

{-- | All valid dice with a six faces and face values up to 1060 -}
mF = 200
validDice :: [[Integer]]
validDice = [[x1,x2,x3,x4,x5,x6] | 
              x1<-[1..mF],
              x2<-[(x1+1)..mF],
              isValidModL [x1,x2],
              x3<-[(x2+1)..mF],
              isValidModL [x1,x2,x3],
              x4<-[(x3+1)..mF],
              isValidModL [x1,x2,x3,x4],
              x5<-[(x4+1)..mF],
              isValidModL [x1,x2,x3,x4,x5],
              x6<-[(x5+1)..mF],
              isValidModL [x1,x2,x3,x4,x5,x6]
            ]
            
findMatch :: [[[Integer]]]
findMatch = [ [xs,ys] | xs <- validDice, ys <-validDice, isValidPrimePair [xs,ys] ] 

validDice5 :: [[Integer]]
validDice5 = [[x1,x2,x3,x4,x5] | 
              x1<-[1..mF],
              x2<-[(x1+1)..mF],
              isValidModL [x1,x2],
              x3<-[(x2+1)..mF],
              isValidModL [x1,x2,x3],
              x4<-[(x3+1)..mF],
              isValidModL [x1,x2,x3,x4],
              x5<-[(x4+1)..mF],
              isValidModL [x1,x2,x3,x4,x5]
            ]

isValidModListEven :: [(Integer, [Integer])] -> Integer -> Bool
isValidModListEven [] _ = True
isValidModListEven ((p,ms):xs) n = (n `mod` p) `elem` ms && isValidModListEven xs n

isValidModListOdd :: [(Integer, [Integer])] -> Integer -> Bool
isValidModListOdd [] _ = True
isValidModListOdd ((p,ms):xs) n = not( (n `mod` p) `elem` ms ) && isValidModListOdd xs n

{-- | findForMod [(2,[0]),(3,[0]),(5,[0,2]),(7,[0,1,2]) -}
validEvenDice :: [(Integer, [Integer])] -> [[Integer]]
validEvenDice xs = [[x1,x2,x3,x4,x5,x6] | 
              x1<-vs,
              x2<-vs,
              x1 < x2,
              isValidModL [x1,x2],
              x3<-vs,
              x2 < x3,
              isValidModL [x1,x2,x3],
              x4<-vs,
              x3 < x4,
              isValidModL [x1,x2,x3,x4],
              x5<-vs,
              x4 < x5,
              isValidModL [x1,x2,x3,x4,x5],
              x6<-vs,
              x5 < x6,
              isValidModL [x1,x2,x3,x4,x5,x6]
            ]
  where vs = filter (isValidModListEven xs) [2,4..mF]

validOddDice :: [(Integer, [Integer])] -> [[Integer]]
validOddDice xs = [[x1,x2,x3,x4,x5,x6] | 
              x1<-vs,
              x2<-vs,
              x1 < x2,
              isValidModL [x1,x2],
              x3<-vs,
              x2 < x3,
              isValidModL [x1,x2,x3],
              x4<-vs,
              x3 < x4,
              isValidModL [x1,x2,x3,x4],
              x5<-vs,
              x4 < x5,
              isValidModL [x1,x2,x3,x4,x5],
              x6<-vs,
              x5 < x6,
              isValidModL [x1,x2,x3,x4,x5,x6]
            ]
  where vs = filter (isValidModListOdd xs) [1,3..mF]
  
findMatchML :: [(Integer, [Integer])] -> [[[Integer]]]
findMatchML ms = [ [xs,ys] | xs <- validEvenDice ms, ys <-validOddDice ms, isValidPrimePair [xs,ys] ] 

findMatchMLAll :: [[[Integer]]]
findMatchMLAll = concat [ findMatchML [(3,[m31]),(5,[m51,m52,m53]),(7,[m71,m72,m73,m74])] |
                            m31 <- [0..2],
                            m51 <- [0..2],
                            m52 <- [m51..3],
                            m53 <- [m52..4],
                            m71 <- [0..3],
                            m72 <- [m71..4],
                            m73 <- [m72..5],
                            m74 <- [m73..6] ]
                            
                            
findMatchMLFirst :: [[Integer]]
findMatchMLFirst =  head ( concat [ findMatchML [(3,[m31]),(5,[m51,m52,m53]),(7,[m71,m72,m73,m74])] |
                            m31 <- [0..2],
                            m51 <- [0..2],
                            m52 <- [m51..3],
                            m53 <- [m52..4],
                            m71 <- [0..3],
                            m72 <- [m71..4],
                            m73 <- [m72..5],
                            m74 <- [m73..6] ]
                         )