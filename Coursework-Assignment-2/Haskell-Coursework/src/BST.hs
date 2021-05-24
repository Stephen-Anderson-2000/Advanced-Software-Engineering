module BST (
    BST,
    BST.lookup
    ) where

data BST item = Leaf
                | Node Int item (BST item) (BST item)

lookup :: Int -> BST item -> Maybe item
lookup soughtKey Leaf = Nothing
lookup soughtKey (Node key item leftChild rightChild)
  | soughtKey < key =
    BST.lookup soughtKey leftChild
  | soughtKey > key =
    BST.lookup soughtKey rightChild
  | otherwise =
    Just item