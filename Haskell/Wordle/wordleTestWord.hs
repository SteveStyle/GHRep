module WordleTestWord where
import WordleUtils

data CharResult = CrFALSE | CrMISMATCH | CrTRUE | CrUNKNOWN
  deriving (Show, Eq, Ord)

type TestResult = [CharResult] -- list of five responses

testWord :: String -> String -> TestResult
testWord xs ys = testMismatch xs (testExact xs ys)
  where
    -- finds characters that match exactly and removes them from the target string.  
    -- test string -> target string -> ( amended target, result list )
    testExact :: String -> String -> (String, TestResult)
    testExact [] _ = ( [], [] )
    testExact (x:xs) (y:ys) | (x==y) = ( as, CrTRUE:bs )
                            | otherwise = ( y:as, CrUNKNOWN:bs )
              where (as,bs) = testExact xs ys

    -- finds characters that exist in different positions, takes the output from testExact
    testMismatch :: String -> ( String, TestResult ) -> TestResult
    testMismatch [] _ = []
    testMismatch (x:xs) (ys, (z:zs)) | (z==CrTRUE)   = CrTRUE     : testMismatch xs (ys, zs) 
                                     | (x `elem` ys) = CrMISMATCH : testMismatch xs ( removeElem x ys, zs) 
                                     | otherwise     = CrFALSE    : testMismatch xs (ys, zs)


