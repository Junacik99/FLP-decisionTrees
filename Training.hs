import Datatypes
import Decisiontree
import Utils


-- Load train data + train tree
training :: IO ()
training = do

    -- Load data
    content <- readFile "test/data/data_table.csv"
    let loaded_data = splitLines ',' $ lines content            -- Raw data
    let train_features = convertData $ map init loaded_data     -- Features
    let train_targets = map last loaded_data                    -- Targets
    let columns_count = length (head train_features)            -- Number of columns
    let feature_indexes = [0..columns_count - 1]                -- List of feature indexes

    

    -- Get min Gini impurity and threshold index for each column
    -- let min_ginis = map (\n -> getGiniIdx train_targets $ getColumn n train_features) [0..columns_count - 1]

    -- let winner_tuple = findMinTuple min_ginis -- (gini impurity, threshold index)

    -- -- (fake feature index, (gini impurity, threshold index/row))
    -- let winner = (elemIndex winner_tuple min_ginis, winner_tuple)

    -- -- Get the winner node (real feature index, threshold)
    -- let winner_node = getNode winner train_features feature_indexes
    -- print winner_node

    -- -- print $ fst winner_tuple

    -- -- Split data into branches
    -- let all_data = zip train_features train_targets
    -- let (left_branch, right_branch) = uncurry splitData winner_node all_data

    -- -- Delete column - get data for new branches
    -- let new_feature_indexes = deleteAt (safeGet $ fst winner) feature_indexes
    -- let new_left_branch =
    --         map (\(features, target) -> (deleteAt (safeGet $ fst winner) features, target)) left_branch
    -- let new_right_branch =
    --         map (\(features, target) -> (deleteAt (safeGet $ fst winner) features, target)) right_branch


    -- print $ map fst new_left_branch

    let all_data = zip train_features train_targets
    let tree = getTree all_data feature_indexes
    print tree

    -- Get ONE column
    -- let column = getColumn 0 train_features

    -- -- Sort targets by feature column
    -- let sorted_data = sortBy (\x y -> compare (fst x) (fst y)) $ zip column train_targets
    -- let sorted_features = map fst sorted_data
    -- let sorted_targets = map snd sorted_data

    -- -- Get weighted Gini impurities and index of the lowest value
    -- let weightedGinis = getWeightedGinis sorted_targets
    -- let gini_idx = (minimum weightedGinis, minIndex weightedGinis)


    -- let tree_test = Node 0 5.5 (Leaf "TridaA") (Node 1 3.0 (Node 2 2.0 (Leaf "TridaA") (Leaf "TridaC")) (Node 2 3.5 (Leaf "TridaB") (Leaf "TridaC")))

    -- Create string from tree and delete last empty line
    let tree_to_save = init $ tree2string 0 tree
    print tree_to_save
    writeFile "output.txt" tree_to_save

    print ""