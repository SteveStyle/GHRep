import Data.Char
import Data.List
import System.IO
import Data.Bits(Bits, (.&.))

intToBits :: Int -> [Int]
intToBits n = reverse (intToBits' n)

intToBits' 0 = [0]
intToBits' 1 = [1]
intToBits' n = (n `mod` 2): intToBits' (n `div` 2) 

bitsToInt :: [Int] -> Int
bitsToInt xs = bitsToInt' (reverse xs)

bitsToInt' [0] = 0
bitsToInt' [1] = 1
bitsToInt' (x:xs) = x + 2 * bitsToInt' xs

isPower2 :: (Bits i, Integral i) => i -> Bool
isPower2 n = n .&. (n-1) == 0


testBitsInt :: [Int] -> String
testBitsInt [] = ""
testBitsInt (n:ns) = (show n)++" "++((show.intToBits) n)++" "++((show.bitsToInt.intToBits) n)++"\n"++(testBitsInt ns)

-- putStrLn testNos
testNos = testBitsInt [35,14,127,129,128,782,783]


solve :: Int -> [Int]
solve 0 = [0]
solve 1 = [1]
solve n = solve' b1 (t-h) (b2:bs)
            where   (b1:b2:bs) = intToBits n
                    h = 1 + sum (b2:bs) 
                    t | h==5 && b2==0    = 16  -- should be 8, change to 16 to test, so 47 fails
                      | h==5 && b2==1    = 16
                      | otherwise       = let e = ceiling (logBase 2 (fromIntegral h)) :: Int in 2^(e) 

solve' :: Int -> Int -> [Int] -> [Int]
solve' a g [] = [a]
solve' a g (b:bs)   | (a==0) || (a > g)     = a : solve' b g bs
                    | otherwise             = solve' (2*a+b) (g-a) bs

type Sol = (Int, [Int], Int, [Int], Int, [[Int]])
soln :: Int -> Sol
soln n = ( n, sout, sum sout, intToBits n, sum (intToBits n), map intToBits sout)
         where    sout = solve n
    
solnN :: Int -> [Sol]    
solnN 1 = [soln 1]              
solnN n = solnN' n []

solnN' :: Int -> [Sol] -> [Sol]
solnN' 1 xs = soln 1 : xs
solnN' n xs = solnN' (n-1) (soln n : xs)
                 
testSolve :: Int -> String
testSolve n     | n<0   = show n ++ " is negative."
                | otherwise     = let   sout = solve n
                                        
                                        t = sum (sout)
                                        sump2q = isPower2 t
                                        
                                        n' = (bitsToInt.concat. map (intToBits)) sout    
                                        digitsq = n==n'
                                  in show n ++ " gives " ++ (show.solve) n ++ ". Digits give n? "++show digitsq++" Sum a power of 2? "++ show sump2q

testSolveB :: Int -> Bool
testSolveB n     | n<0   = False
                | otherwise     = let   sout = solve n
                                        
                                        t = sum (sout)
                                        sump2q = isPower2 t
                                        
                                        n' = (bitsToInt.concat. map (intToBits)) sout    
                                        digitsq = n==n'
                                  in digitsq && sump2q

testMax :: Int -> Bool
testMax 0 = True
testMax n = all testSolveB [1..n]