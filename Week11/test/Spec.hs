import Test.HUnit
-- https://hackage.haskell.org/package/HUnit
import Luhn

main :: IO ()
main = do
    luhnDoubleResults <- runTestTT luhnDoubleTests
    print luhnDoubleResults

test1 :: Test
test1 = TestCase (assertEqual "LuhnDouble 0" 0 (luhnDouble 0))

test2 :: Test
test2 = TestCase (assertEqual "LuhnDouble 4" 8 (luhnDouble 4))

test3 :: Test
test3 = TestCase (assertEqual "LuhnDouble 5" 1 (luhnDouble 5))

test4 :: Test
test4 = TestCase (assertEqual "LuhnDouble 9" 9 (luhnDouble 9))

luhnDoubleTests :: Test 
luhnDoubleTests = TestList [test1, test2, test3, test4]
