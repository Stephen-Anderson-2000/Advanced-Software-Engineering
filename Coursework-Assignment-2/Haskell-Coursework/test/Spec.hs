import Test.QuickCheck
import BinaryTree
import Data.List
import qualified Data.Maybe


-- Function taken from Week 20 demonstration
qsort :: [Int] -> [Int]
qsort [] = []
qsort (pivot : restOfList) =
  let smallerItems = filter (< pivot) restOfList
      largerItems = filter (>= pivot) restOfList
   in
     qsort smallerItems ++ [pivot] ++ qsort largerItems

-- Function taken from https://stackoverflow.com/questions/1735146/ways-to-get-the-middle-of-a-list-in-haskell
middle :: [a] -> [a]
middle xs = take (signum ((l + 1) `mod` 2) + 1) $ drop ((l - 1) `div ` 2) xs
  where l = length xs




prop_bst_create_empty :: Bool
prop_bst_create_empty = do
    let tree = newEmptyBST
    -- if it reaches the end of the property then it passes
    -- (it didn't crash when making the data structure)
    True


prop_bst_create_from_item :: Int -> String -> Bool
prop_bst_create_from_item key value = do
    let tree = newBSTFromItem key value in
        lookupKey key tree == Just value


prop_bst_create_from_list :: [(Int, String)] -> Bool
prop_bst_create_from_list items = do
    let tree = newBSTFromList items in
        null items ||
        (if length items < 2 then
            lookupKey (fst (head items)) tree == Just (snd (head items))
        else
            (lookupKey (fst (last items)) tree == Just (snd (last items)))
            && Data.Maybe.isJust (lookupKey (fst (head items)) tree)
            -- in case the first value is completely overwritten and the key
            -- can no longer be found
        )


prop_bst_insert_item :: Int -> String -> Bool
prop_bst_insert_item key value =
    let tree = addItem key value newEmptyBST in
        lookupKey key tree == Just value


prop_bst_insert_items :: [(Int, String)] -> Bool
prop_bst_insert_items items = do
    let tree = addList items newEmptyBST in
        null items ||
        (if length items < 2 then
            lookupKey (fst (head items)) tree == Just (snd (head items))
        else
            (lookupKey (fst (last items)) tree == Just (snd (last items)))
            && Data.Maybe.isJust (lookupKey (fst (head items)) tree)
        )


prop_bst_inserted_items_sorted :: [(Int, String)] -> Bool
prop_bst_inserted_items_sorted items =
    null items || (
        let tree = newBSTFromList items in
            let list = bstToList tree [] in
                qsort list == list
        )


prop_bst_insert_overwrites :: [String] -> Bool
prop_bst_insert_overwrites values =
    null values || (
        let keys = replicate (length values) 0 in
            let tree = newBSTFromList (zip keys values) in
                if length values > 2 && (head values /= last values) then
                    (lookupKey 0 tree /= Just (head values))
                    && (lookupKey 0 tree == Just (last values))
                else
                    lookupKey 0 tree == Just (last values)
        )


prop_bst_lookup_nonexistent_items :: [(Int, String)] -> [Int] -> Bool
prop_bst_lookup_nonexistent_items items list =
    null items || (
        lookupKey (fst (head items)) testTree /= Just (snd (head items)))


prop_bst_lookup_all_items :: [(Int, String)] -> Bool
prop_bst_lookup_all_items items = 
    False


prop_bst_print_in_order :: [(Int, String)] -> Bool
prop_bst_print_in_order items =
    False


prop_bst_delete_nonexistent_item :: [(Int, String)] -> Bool
prop_bst_delete_nonexistent_item items =
    null items || (
        let tree = deleteItem (fst (head items)) testTree in
            lookupKey (fst (head items)) tree /= Just (snd (head items))
        )


prop_bst_delete_head :: [(Int, String)] -> Bool
prop_bst_delete_head items =
    null items || (
        let tree = deleteItem (fst (head items)) (newBSTFromList items) in
            if fst (head items) /= fst (last items) then
                lookupKey (fst (head items)) tree /= Just (snd (head items))
                && Data.Maybe.isJust (lookupKey (fst (last items)) tree)
            else
                lookupKey (fst (head items)) tree /= Just (snd (head items))
            )

prop_bst_delete_last :: [(Int, String)] -> Bool
prop_bst_delete_last items =
    null items || (
        let tree = deleteItem (fst (last items)) (newBSTFromList items) in
            lookupKey (fst (last items)) tree /= Just (snd (last items))
        )


prop_bst_delete_middle :: [(Int, String)] -> Bool 
prop_bst_delete_middle items = 
    null items || (
        let tree = deleteItem (fst (head (middle items))) (newBSTFromList items) in
            lookupKey (fst (head (middle items))) tree /= Just (snd (head (middle items)))
        )


prop_bst_delete_all_individually :: [(Int, String)] -> Bool
prop_bst_delete_all_individually items =
    False


main :: IO ()
main = do
    -- print() neatens up the console when displaying the results
    print()

    quickCheck prop_bst_create_empty --passes
    quickCheck prop_bst_create_from_item --passes
    quickCheck prop_bst_create_from_list --passes

    quickCheck prop_bst_insert_item --passes
    quickCheck prop_bst_insert_items --passes
    quickCheck prop_bst_inserted_items_sorted --passes
    quickCheck prop_bst_insert_overwrites --passes

    quickCheck prop_bst_lookup_nonexistent_items --passes
    --quickCheck prop_bst_lookup_all_items --test not implemented

    --quickCheck prop_bst_print_in_order --test not implemented

    quickCheck prop_bst_delete_nonexistent_item --passes
    quickCheck prop_bst_delete_head --passes
    quickCheck prop_bst_delete_last --passes
    quickCheck prop_bst_delete_middle --passes
    --quickCheck prop_bst_delete_all_individually --test not implemented