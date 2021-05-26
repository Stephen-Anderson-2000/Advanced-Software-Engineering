import Test.QuickCheck
import BinaryTree
import Data.List

prop_bst_create_empty :: Bool
prop_bst_create_empty = do
    let tree = newEmptyBST
    -- if it reaches the end of the property then it passes
    -- (it didn't crash when making the data structure)
    True


prop_bst_create_from_item :: Int -> String -> Bool 
prop_bst_create_from_item key value = do 
    let tree = newBSTFromItem key value
    lookupBST key tree == Just value


prop_bst_create_from_list :: [(Int, String)] -> Bool 
prop_bst_create_from_list items = do
    let tree = newBSTFromList items
    False


prop_bst_insert_item :: Int -> String -> Bool
prop_bst_insert_item key value = do
    let tree = addItem key value newEmptyBST
    lookupBST key tree == Just value


prop_bst_insert_items :: [(Int, String)] -> Bool
prop_bst_insert_items items = False


prop_bst_inserted_items_sorted :: [(Int, String)] -> Bool 
prop_bst_inserted_items_sorted items = False


prop_bst_insert_overwrites :: [(Int, String)] -> Bool 
prop_bst_insert_overwrites items = False


prop_bst_lookup_nonexistent_items :: [(Int, String)] -> [Int] -> Bool 
prop_bst_lookup_nonexistent_items items list = False


prop_bst_lookup_item :: [(Int, String)] -> Bool
prop_bst_lookup_item items = False


prop_bst_lookup_all_items :: [(Int, String)] -> Bool 
prop_bst_lookup_all_items items = False


prop_bst_print_in_order :: [(Int, String)] -> Bool 
prop_bst_print_in_order items = False


prop_bst_delete_item :: [(Int, String)] -> Bool 
prop_bst_delete_item items = False 


prop_bst_delete_reverse_order :: [(Int, String)] -> Bool 
prop_bst_delete_reverse_order items = False


prop_bst_delete_all :: [(Int, String)] -> Bool 
prop_bst_delete_all items = False


main :: IO ()
main = do
    quickCheck prop_bst_create_empty
    quickCheck prop_bst_create_from_item
    quickCheck prop_bst_create_from_list
    quickCheck prop_bst_insert_item
    quickCheck prop_bst_insert_items
    quickCheck prop_bst_inserted_items_sorted
    quickCheck prop_bst_insert_overwrites
    quickCheck prop_bst_lookup_nonexistent_items
    quickCheck prop_bst_lookup_item
    quickCheck prop_bst_lookup_all_items
    quickCheck prop_bst_print_in_order
    quickCheck prop_bst_delete_item
    quickCheck prop_bst_delete_reverse_order
    quickCheck prop_bst_delete_all