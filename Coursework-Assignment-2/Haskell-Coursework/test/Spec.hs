import Test.QuickCheck

import BST
import Data.List

prop_bst_create_empty :: () -> Bool 
prop_bst_create_empty dict = False

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

prop_bst_convert_to_list :: [(Int, String)] -> Bool 
prop_bst_convert_to_list items = False 

prop_bst_delete_item :: [(Int, String)] -> Bool 
prop_bst_delete_item items = False 

prop_bst_delete_reverse_order :: [(Int, String)] -> Bool 
prop_bst_delete_reverse_order items = False

prop_bst_delete_all :: [(Int, String)] -> Bool 
prop_bst_delete_all items = False

main :: IO ()
main = do
    quickCheck prop_bst_create_empty
    quickCheck prop_bst_inserted_items_sorted
    quickCheck prop_bst_insert_items
    quickCheck prop_bst_lookup_item