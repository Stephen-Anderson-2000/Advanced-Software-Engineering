module RecursiveDataTypes where

import Prelude hiding (lookup)

-- data Link = Node Int Link
--           | Null

-- data List = List Link

-- mylist :: List
-- mylist = List (Node 1 (Node 2 (Node 3 Null)))

-- listLength :: List -> Int
-- listLength (List hd) = lengthRec hd

-- lengthRec :: Link -> Int
-- lengthRec Null        = 0
-- lengthRec (Node i tl) = 1 + lengthRec tl

data List = Node Int List
          | Null

mylist :: List
mylist = Node 1 (Node 2 (Node 3 Null))

lengthRec :: List -> Int
lengthRec Null        = 0
lengthRec (Node i tl) = 1 + lengthRec tl

-----------------------------------------------------

data BST = Leaf
         | InternalNode Int String BST BST

data MaybeString = JustString String
                 | NoString

lookup :: Int -> BST -> MaybeString
lookup soughtKey Leaf = NoString
lookup soughtKey (InternalNode key item leftChild rightChild) =
  if soughtKey < key then
    lookup soughtKey leftChild
  else if soughtKey > key then
    lookup soughtKey rightChild
  else
    JustString item
