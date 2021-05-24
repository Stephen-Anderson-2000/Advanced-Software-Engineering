module BST
    ( 
        dummyFunc
    ) where

-- Tree item can either be a leaf (null end point)
-- or it can be a node that contains an item and pointers to the two child items
data BinaryTree item = Leaf 
                    | Node Int item (BinaryTree item) (BinaryTree item)

dummyFunc :: IO ()
dummyFunc = undefined