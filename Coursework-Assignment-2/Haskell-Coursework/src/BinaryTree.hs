module BinaryTree (
    BST,
    newEmptyBST,
    newBSTFromItem,
    newBSTFromList,
    addItem,
    lookupBST
    ) where

data BST item = Leaf
                | Node Int item (BST item) (BST item)
                deriving Show

-- Method and structure taken from http://learnyouahaskell.com/zippers
testTree :: BST String
testTree =
    Node 22 "Mary"
        (Node 0 "Harry"
            (Node 4 "Steve" 
                (Node (-1) "Ed" Leaf Leaf)  
                (Node 1 "Will" Leaf Leaf)  
            )  
            (Node 9 "Ed"  
                Leaf  
                (Node 19 "Henry" Leaf Leaf)  
            )  
        )  
        (Node 37 "Vicky"  
            (Node 26 "Charlie"  
                (Node 24 "Jim" 
                    (Node 23 "Liz" Leaf Leaf) 
                    Leaf
                )  
                (Node 31 "Anne" Leaf Leaf)  
            )
            (Node 42 "John" Leaf Leaf)
        )


newEmptyBST :: BST item
newEmptyBST = Leaf


newBSTFromItem :: Int -> item -> BST item
newBSTFromItem key value = Node key value Leaf Leaf


newBSTFromList :: [(Int, item)] -> BST item
newBSTFromList list = newEmptyBST
-- currently not implemented as it needs to use addItem?


addItem :: Int -> item -> BST item -> BST item
addItem key value Leaf = Node key value Leaf Leaf
addItem key value tree@(Node rootKey rootItem leftChild rightChild)
    | key < rootKey =
        addItem key value leftChild
    | key > rootKey =
        addItem key value rightChild
    | key == rootKey =
        Node key value leftChild rightChild
    | otherwise =
        tree
        

lookupBST :: Int -> BST item -> Maybe item
lookupBST soughtKey Leaf = Nothing
lookupBST soughtKey (Node key item leftChild rightChild)
  | soughtKey < key =
    lookupBST soughtKey leftChild
  | soughtKey > key =
    lookupBST soughtKey rightChild
  | otherwise =
    Just item