import Test.HUnit
-- https://hackage.haskell.org/package/HUnit
import Lib

main :: IO ()
main = do
    results <- runTestTT myTestSuite
    print results

-- Test case takes the assertion type, the identifier, the expected result then the function and parameter(s)
test1 :: Test
test1 = TestCase (assertEqual "Square of 0" 0 (sqr 0))

test2 :: Test
test2 = TestCase (assertEqual "Square of 1" 1 (sqr 1))

test3 :: Test
test3 = TestCase (assertEqual "Square of 2" 4 (sqr 2))


myTestSuite :: Test 
myTestSuite = TestList [test1, test2, test3]