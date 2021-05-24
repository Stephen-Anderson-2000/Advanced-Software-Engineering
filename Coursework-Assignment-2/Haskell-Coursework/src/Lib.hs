module Lib where

qsort :: [Int] -> [Int]
qsort [] = []
qsort (pivot : restOfList) = 
    let smallerItems = filter (< pivot) restOfList
        largerItems = filter (> pivot) restOfList
    in
        qsort smallerItems ++ [pivot] ++ qsort largerItems