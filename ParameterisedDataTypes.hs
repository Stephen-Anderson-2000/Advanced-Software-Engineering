module ParameterisedDataTypes where

import Prelude hiding (lookup)

data List t = Node t (List t)
            | Null

mylist :: List Int
mylist = Node 1 (Node 2 (Node 3 Null))

lengthRec :: List t -> Int
lengthRec Null        = 0
lengthRec (Node i tl) = 1 + lengthRec tl

-- data [t] = t : [t]
--          | []

-- mylist :: [Int]
-- mylist = 1 : 2 : 3 : []

-- lengthRec :: [t] -> Int
-- lengthRec []       = 0
-- lengthRec (i : tl) = 1 + lengthRec tl

-----------------------------------------------------

data BST item = Leaf
              | InternalNode Int item (BST item) (BST item)

-- data Maybe item = Just item
--                 | Nothing

lookup :: Int -> BST item -> Maybe item
lookup soughtKey Leaf = Nothing
lookup soughtKey (InternalNode key item leftChild rightChild) =
  if soughtKey < key then
    lookup soughtKey leftChild
  else if soughtKey > key then
    lookup soughtKey rightChild
  else
    Just item
