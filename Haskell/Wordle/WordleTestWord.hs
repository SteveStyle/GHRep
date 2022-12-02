module WordleTestWord where
import WordleUtils

data CharResult = CrFALSE | CrMISMATCH | CrTRUE | CrUNKNOWN
  deriving (Eq, Ord)
  
instance Show CharResult where
  show CrFALSE    = "_"
  show CrMISMATCH = "M"
  show CrTRUE     = "T"
  show CrUNKNOWN  = "U"
  
s2cr :: String -> [CharResult]
s2cr [] = []
s2cr ('_':xs) = CrFALSE: (s2cr xs)
s2cr (' ':xs) = CrFALSE: (s2cr xs)
s2cr ('M':xs) = CrMISMATCH: (s2cr xs)
s2cr ('O':xs) = CrMISMATCH: (s2cr xs)
s2cr ('T':xs) = CrTRUE: (s2cr xs)
s2cr ('G':xs) = CrTRUE: (s2cr xs)
s2cr ('U':xs) = CrUNKNOWN: (s2cr xs)
  

type TestResult = [CharResult] -- list of five responses

--instance Show TestResult where
--  show ts = concat [ show x | x <- ts ]

testWord :: String -> String -> TestResult
testWord xs ys = testMismatch xs (testExact xs ys)
  where
    -- finds characters that match exactly and removes them from the target string.  
    -- test string -> target string -> ( amended target, result list )
    testExact :: String -> String -> (String, TestResult)
    testExact [] _ = ( [], [] )
    testExact (x:xs) (y:ys) | (x==y)    = ( as, CrTRUE:bs )
                            | otherwise = ( y:as, CrUNKNOWN:bs )
              where (as,bs) = testExact xs ys

    -- finds characters that exist in different positions, takes the output from testExact
    testMismatch :: String -> ( String, TestResult ) -> TestResult
    testMismatch [] _ = []
    testMismatch (x:xs) (ys, (z:zs)) | (z==CrTRUE)   = CrTRUE     : testMismatch xs (ys, zs) 
                                     | (x `elem` ys) = CrMISMATCH : testMismatch xs ( removeElem x ys, zs) 
                                     | otherwise     = CrFALSE    : testMismatch xs (ys, zs)


