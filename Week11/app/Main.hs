module Main where

import Luhn ( luhnChecksum )

main :: IO ()
main = do
    luhnChecksum