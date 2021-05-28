module BinaryTree (
    BST,
    newEmptyBST,
    testTree,
    newBSTFromItem,
    newBSTFromList,
    addItem,
    addList,
    deleteItem,
    lookupKey,
    bstToList,
    displayBST,
    ) where

data BST item = Leaf
                | Node Int item (BST item) (BST item)
                deriving Show

-- Method and structure taken from http://learnyouahaskell.com/zippers
-- Values used are from earlier lab work
testTree :: BST String
testTree =
    Node 22 "Mary"
        (Node 0 "Harry"
            (Node 4 "Steve"
                (Node (-1) "Ed" Leaf Leaf)
                (Node 1 "Will" Leaf Leaf)
            )
            (Node 9 "Joseph"
                Leaf
                (Node 19 "Henry" Leaf Leaf)
            )
        )
        (Node 37 "Vicky"
            (Node 26 "Charley"
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


newBSTFromList :: Eq item => [(Int, item)] -> BST item
newBSTFromList [] = newEmptyBST
newBSTFromList items = addList items newEmptyBST


addItem :: Int -> item -> BST item -> BST item
addItem key item Leaf =
    Node key item Leaf Leaf
addItem key item tree@(Node rootKey rootItem left right)
    | key == rootKey =
        Node key item left right
    | key < rootKey =
        Node rootKey rootItem (addItem key item left) right
    | key > rootKey =
        Node rootKey rootItem left (addItem key item right)


addList :: Eq item => [(Int, item)] -> BST item -> BST item
addList [] tree = tree
addList items Leaf =
    addList (tail items) (uncurry addItem (head items) newEmptyBST)
addList items tree =
     addList (tail items) (uncurry addItem (head items) tree)


lookupKey :: Int -> BST item -> Maybe item
lookupKey soughtKey Leaf = Nothing
lookupKey soughtKey (Node key item leftChild rightChild)
    | soughtKey < key =
        lookupKey soughtKey leftChild
    | soughtKey > key =
        lookupKey soughtKey rightChild
    | otherwise =
        Just item


deleteItem :: Int -> BST item -> BST item
deleteItem key Leaf = Leaf
deleteItem key node@(Node rootKey rootItem left right)
    | key == rootKey =
        removeNode node
    | key < rootKey =
        Node rootKey rootItem (deleteItem key left) right
    | key > rootKey =
        Node rootKey rootItem left (deleteItem key right)


removeNode :: BST item -> BST item
removeNode node@(Node key item Leaf Leaf) = Leaf
removeNode node@(Node key item Leaf right) = right
removeNode node@(Node key item left Leaf) = left
removeNode node@(Node key item left right) =
    Node newKey newItem left right where
        newValues = minimumNode right
        newKey = fst newValues
        newItem = snd newValues


-- minimum node is never actually deleted leading to duplicates
-- tests do not currently reflect this
minimumNode :: BST item -> (Int, item)
minimumNode node@(Node key item Leaf right) = (key, item)
minimumNode node@(Node _ _ left _) = minimumNode left


nodeToString :: Show item => BST item -> String
nodeToString node@(Node key item left right) =
    show key ++ ", " ++ show item


bstToList :: Eq item => BST item -> [Int] -> [Int]
bstToList tree@(Node key item left right) list =
    list ++ bstToList left list
    ++ [key]
    ++ bstToList right list
bstToList Leaf list = list


displayBST :: Show item => BST item -> IO ()
displayBST Leaf = return ()
displayBST tree@(Node key item left right) = do
    displayBST left
    putStrLn (nodeToString tree)
    displayBST right