module Luhn
    ( luhnChecksum
    ) where

import Data.Char(digitToInt)

luhnDouble :: Int -> Int
luhnDouble a = 
    let x = a * 2 in
        if x > 9 then x - 9 else x

luhnAlgorithm :: Int -> Int -> Int -> Int -> Bool 
luhnAlgorithm a b c d =
    let  x = a + b + c + d in 
        mod x 10 == 0

getUserChar :: IO Char
getUserChar = do
    getChar       

luhnChecksum :: IO ()
luhnChecksum = do
    putStrLn "Please enter the 4 digits from your bank card that you want checking:"
    a <- getUserChar
    b <- getUserChar
    c <- getUserChar
    d <- getUserChar
    putStrLn(boolToString(luhnAlgorithm(digitToInt a) (digitToInt b) (digitToInt c) (digitToInt d)))

boolToString :: Bool -> String 
boolToString b = 
    if b then "True" else "False"