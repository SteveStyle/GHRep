module Primes where

-- (eq. to) find2km (2^k * n) = (k,n)
find2km :: Integral a => a -> (a,a)
find2km n = f 0 n
    where 
        f k m
            | r == 1 = (k,m)
            | otherwise = f (k+1) q
            where (q,r) = quotRem m 2        

-- n is the number to test; a is the (presumably randomly chosen) witness
millerRabinPrimality :: Integer -> Integer -> Bool
millerRabinPrimality n a
    | a <= 1 || a >= n-1 = 
        error $ "millerRabinPrimality: a out of range (" 
              ++ show a ++ " for "++ show n ++ ")" 
    | n < 2 = False
    | even n = False
    | b0 == 1 || b0 == n' = True
    | otherwise = iter (tail b)
    where
        n' = n-1
        (k,m) = find2km n'
        b0 = powMod n a m
        b = take (fromIntegral k) $ iterate (squareMod n) b0
        iter [] = False
        iter (x:xs)
            | x == 1 = False
            | x == n' = True
            | otherwise = iter xs

-- (eq. to) pow' (*) (^2) n k = n^k
pow' :: (Num a, Integral b) => (a->a->a) -> (a->a) -> a -> b -> a
pow' _ _ _ 0 = 1
pow' mul sq x' n' = f x' n' 1
    where 
        f x n y
            | n == 1 = x `mul` y
            | r == 0 = f x2 q y
            | otherwise = f x2 q (x `mul` y)
            where
                (q,r) = quotRem n 2
                x2 = sq x
 
mulMod :: Integral a => a -> a -> a -> a
mulMod a b c = (b * c) `mod` a
squareMod :: Integral a => a -> a -> a
squareMod a b = (b * b) `rem` a

-- (eq. to) powMod m n k = n^k `mod` m
powMod :: Integral a => a -> a -> a -> a
powMod m = pow' (mulMod m) (squareMod m)

-- Steve Morris additions
-- According to Wikipedia this test may produce false positives, hence it is normal to test with more than one witness.
-- If any witness says compisite then the number is composite.  
-- But it is possible for a witness to be a strong liar, and fail to declare a composite number as composite.
-- Maple uses 2,3,5,7,11 as witnesses which guarentees accuracy up to some limit
{-- | copy the Maple isPrime function, which can sometimes give a false positive, showing a compisite number as prime.  
These pseudoprimes are rare and large numbers. 
The witnesses used can be adjusted based on the size of n if performance is an issue.

if n < 2,047, it is enough to test a = 2;
if n < 1,373,653, it is enough to test a = 2 and 3;
if n < 9,080,191, it is enough to test a = 31 and 73;
if n < 25,326,001, it is enough to test a = 2, 3, and 5;
if n < 3,215,031,751, it is enough to test a = 2, 3, 5, and 7;
if n < 4,759,123,141, it is enough to test a = 2, 7, and 61;
if n < 1,122,004,669,633, it is enough to test a = 2, 13, 23, and 1662803;
if n < 2,152,302,898,747, it is enough to test a = 2, 3, 5, 7, and 11;
if n < 3,474,749,660,383, it is enough to test a = 2, 3, 5, 7, 11, and 13;
if n < 341,550,071,728,321, it is enough to test a = 2, 3, 5, 7, 11, 13, and 17.
if n < 3,825,123,056,546,413,051, it is enough to test a = 2, 3, 5, 7, 11, 13, 17, 19, and 23.
if n < 18,446,744,073,709,551,616 = 2^64, it is enough to test a = 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, and 37.
if n < 318,665,857,834,031,151,167,461, it is enough to test a = 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, and 37.
if n < 3,317,044,064,679,887,385,961,981, it is enough to test a = 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, and 41.
 -}
isPrimeP :: Integer -> Bool
isPrimeP (-3) = True
isPrimeP (-2) = True
isPrimeP (-1) = False
isPrimeP 0 = False
isPrimeP 1 = False
isPrimeP 2 = True
isPrimeP 3 = True
isPrimeP n = all (millerRabinPrimality an) witnesses
  where an = abs n
        witnesses   | an < 2047                      =[2]
                    | an < 1373653                   =[2,3]
                    | an < 9080191                   =[31,73]
                    | an < 25326001                  =[2,3,5]
                    | an < 3215031751                =[2,3,5,7]
                    | an < 4759123141                =[2,7,61]
                    | an < 2152302898747             =[2,3,5,7,11]
                    | an < 3474749660383             =[2,3,5,7,11,13]
                    | an < 341550071728321           =[2,3,5,7,11,13,17]
                    | an < 3825123056546413051       =[2,3,5,7,11,13,17,19,23]
                    | an < 318665857834031151167461  =[2,3,5,7,11,13,17,19,23,29,31,37]
                    | an < 3317044064679887385961981 =[2,3,5,7,11,13,17,19,23,29,31,37,41]
                    | otherwise                      =[2,3,5,7,11,13,17,19,23,29,31,37,41]
                    
