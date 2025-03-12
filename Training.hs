{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use guards" #-}
{-# HLINT ignore "Redundant if" #-}
import Datatypes
import Decisiontree
import Utils
import Data.List (
    sort,
    nub,
    elemIndex,
    sortBy,
    minimumBy,
    maximumBy,
    group,
    partition
    )
import Data.Ord (comparing)

-- Obtain column of features with index i
getColumn i = map (!! i)

-- Get average values for adjacent points in one column
getIntermediateValues [] = []
getIntermediateValues [x] = []
getIntermediateValues (x:y:xs) = (x + y) / 2.0 : getIntermediateValues (y:xs)

initCounter classes = replicate (length classes) 0

-- Get number of targets under threshold
getUnderThresholdTargets targets classes = init $ tail $ scanl updateCounter (initCounter classes) targets
    where updateCounter counter target =
            case elemIndex target classes of
                Just idx -> take idx counter ++ [counter !! idx + 1] ++ drop (idx + 1) counter
                Nothing  -> counter  -- If the target is not in the classes list, leave the counter unchanged

-- Calculate Gini impurity
giniImpurity targets = 1.0 - sum (map (\x -> (x / sum targets) ^ 2) targets)

-- Calculate weighted average of Gini impurity
weightedGiniImpurity under over = (sum under / (sum under + sum over)) * giniImpurity under + (sum over / (sum under + sum over)) * giniImpurity over

-- Get index of the minimum element in the list
minIndex [] = 0
minIndex xs = head $ filter (\i -> xs !! i == minimum xs) [0..length xs - 1]

-- Get candidate threshold for one column
-- getCandidateThreshold col_idx features targets = intermediate_values !! minIndex weighted_gini
--     where
--         -- Get intermediate values for one column
--         intermediate_values = getIntermediateValues $ getColumn col_idx features

--         -- Calculate weighted Gini impurity for each threshold
--         weighted_gini = zipWith weightedGiniImpurity under_threshold over_threshold

--         -- Get under and over threshold counts
--         under_threshold = getUnderThresholdTargets targets unique_targets
--         over_threshold = reverse $ getUnderThresholdTargets (reverse targets) unique_targets

--         -- List of unique targets
--         unique_targets = sort $ nub targets


-- Get weighted Gini impurities for one column
getWeightedGinis targets = zipWith weightedGiniImpurity under_threshold over_threshold
    where
        -- Get under and over threshold counts
        under_threshold = getUnderThresholdTargets targets unique_targets
        over_threshold = reverse $ getUnderThresholdTargets (reverse targets) unique_targets

        -- List of unique targets
        unique_targets = sort $ nub targets

-- getMinGinis features targets = map ( $ sort) features
--     where
--         -- Get under and over threshold counts
--         under_threshold = getUnderThresholdTargets targets unique_targets
--         over_threshold = reverse $ getUnderThresholdTargets (reverse targets) unique_targets

--         -- List of unique targets
--         unique_targets = sort $ nub targets


-- Get Gini impurity index for one column
getGiniIdx targets column = (minimum weightedGinis, minIndex weightedGinis)
        where
            weightedGinis = getWeightedGinis sorted_targets

            -- Sort targets by feature column
            sorted_data = sortBy (\x y -> compare (fst x) (fst y)) $ zip column targets
            sorted_targets = map snd sorted_data

-- In a list of tuples (gini impurity, feature index), find the tuple with lowest gini impurity
findMinTuple = minimumBy (\x y -> compare (fst x) (fst y))

-- Get node value
getNode (Just feature_idx, (_, threshold_idx)) features feature_indexes = (real_feature_idx, threshold)
    -- This is the threshold that will be used as a root node (snd (snd winner) is the feature index)
    where
        threshold = getIntermediateValues (getColumn feature_idx features) !! threshold_idx
        real_feature_idx = feature_indexes !! feature_idx
getNode (Nothing, _) _ _ = error "No Node"

isPure targets = length (nub targets) == 1      -- Check if there is only one unique target in the list
isPure' xs = length (filter (/= 0) xs) == 1     -- Check if there is only one non-zero element in the list

-- Get majority target in the list
getMajority targets = head $ maximumBy (comparing length) $ group $ sort targets

-- Splits data at feature idx by threshold
splitData idx threshold = partition predicate
    where
        predicate (features, _) = features !! idx <= threshold

-- Delete element at index n
deleteAt _ [] = []
deleteAt 0 (x:xs) = xs
deleteAt n (x:xs) = x : deleteAt (n - 1) xs

-- Get value from Maybe
safeGet (Just x) = x
safeGet Nothing = error "No value"


getTree :: [([ Feature ], Class)] -> [Int] -> Tree
getTree all_data feature_indexes =
    -- If the branch is pure, return Leaf with the class
    if isPure $ map snd all_data
        then Leaf $ getMajority $ map snd all_data

    -- If max depth is reached, return Leaf with the class that has majority
    else if null feature_indexes
        then Leaf $ getMajority $ map snd all_data

    -- If the branch is not pure, return Node with the feature index and threshold
    else Node getIndex getThreshold (getTree new_left_branch new_feature_indexes) (getTree new_right_branch new_feature_indexes)
    where
        -- Get min Gini impurity and threshold index for each column
        columns_count = length feature_indexes
        min_ginis = map (\n -> getGiniIdx (map snd all_data) $ getColumn n $ map fst all_data) [0..columns_count - 1]

        -- (gini impurity, threshold index)
        winner_tuple = findMinTuple min_ginis

        -- (fake feature index, (gini impurity, threshold index/row))
        winner = (elemIndex winner_tuple min_ginis, winner_tuple)

        -- Get the winner node (real feature index, threshold)
        features = map fst all_data
        winner_node = getNode winner features feature_indexes

        -- Split data into branches
        (left_branch, right_branch) = splitData (safeGet $ fst winner) (snd winner_node) all_data

        -- Delete column - get data for new branches
        new_feature_indexes = deleteAt (safeGet $ fst winner) feature_indexes

        new_left_branch =
            map (\(features, target) -> (deleteAt (safeGet $ fst winner) features, target)) left_branch
        new_right_branch =
            map (\(features, target) -> (deleteAt (safeGet $ fst winner) features, target)) right_branch

        -- Get index of the winner node
        getIndex = fst winner_node

        -- Get threshold of the winner node
        getThreshold = snd winner_node

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
    -- let train_data = zip train_features train_targets           -- Data (features + targets)
    -- let unique_targets = sort $ nub train_targets               -- List of unique targets
    -- print unique_targets
    -- print train_features
    -- print train_targets

    -- TODO: Train tree
    -- For each column:
        -- 1. sort column                                                       check
        -- 2. get average intermediate values (between values)                  check
        -- 3. calculate Gini impurity for each of them - more calculations      check
            -- a. for both branches -> 1 - probability of all classes squared   check
            -- b. total gini impurity -> weighted average from two branches     check
            -- c. select threshold with the lowest gini impurity                check
        -- 4. select column (feature) with the lowest gini impurity             check

    -- Get min Gini impurity and threshold index for each column
    -- let min_ginis = map (\n -> getGiniIdx train_targets $ getColumn n train_features) [0..columns_count - 1]

    -- let winner_tuple = findMinTuple min_ginis -- (gini impurity, threshold index)

    -- -- (fake feature index, (gini impurity, threshold index/row))
    -- let winner = (elemIndex winner_tuple min_ginis, winner_tuple)

    -- -- Get the winner node (real feature index, threshold)
    -- let winner_node = getNode winner train_features feature_indexes
    -- print winner_node

    -- -- print $ fst winner_tuple

    -- -- Split data:
    --     -- 1. Get under threshold and over threshold
    --     -- 2. Delete the winner column? Or skip it? Remember, the column index will be needed
    --         -- Keep list of indexes. On delete, delete also from the list of indexes.
    --         -- New column index will point to the true index in the original list
    -- -- Recursively call for each branch
    -- -- Count classes in the branch
    --     -- Get under threshold or over threshold and call isPure'
    --     -- Or after split call isPure
    -- -- If the branch is pure, return Leaf with the class
    -- -- How to find out if the branch is pure? -> Check if all targets are the same
    -- -- If max depth is reached, return Leaf with the class that has majority
    --     -- Max depth = columns_count (in recursive call, decrease the depth)
    -- -- If the branch is not pure, return Node with the feature index and threshold

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

    -- let tree = uncurry Node winner_node (getTree new_left_branch new_feature_indexes) (getTree new_right_branch new_feature_indexes)
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
    -- let tree_to_save = init $ tree2string 0 tree_test
    -- print tree_to_save
    -- writeFile "output.txt" tree_to_save

    print ""