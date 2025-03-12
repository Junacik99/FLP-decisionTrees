{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use guards" #-}
{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Use first" #-}
module Decisiontree
(
    predict,
    predictTree,
    loadTree,
    tree2string,
    getTree
)
where
---- DECISION TREE ----
-- This module contains all functionality of the decision tree:
--      Traversing
--      Predictions
--      TODO: Training
import Datatypes
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

--- PREDICTIONS
-- Predict a single data point or decide based on feature and threshold
predict (Leaf c) _ = c
predict (Node idx threshold ltree rtree) features
    | features !! idx <= threshold = predict ltree features
    | otherwise = predict rtree features

-- Make predictions for all input data
predictTree tree = map (predict tree)


--- LOADING TREE
-- Create tree from lines
loadTree lines = processTree $ trimLines ' ' $ getLevels lines

-- Process tree node after node
processTree [] = error "Empty Tree"
processTree (('L':xs, _):_) = Leaf (splitOn ' ' xs !! 1)
processTree (('N':xs, d):rest) =
    Node (getIdx xs) (getThreshold xs) (processTree rest) (processTree (getRightTree (tail rest)))
        where
            getRightTree [] = []
            getRightTree xs = dropWhileSecond (>d+1) xs -- delete all until second occurence of d+1

-- Get feature index of the node
getIdx node = read (head (splitOn ',' (splitOn ':' node !! 1))) :: Index

-- Get threshold value of the node
getThreshold node = read (splitOn ',' (splitOn ':' node !! 1) !! 1) :: Feature

-- Obtain levels/depth of nodes
getLevels tree = zip tree (map f tree)
    where f line = countChar ' ' line `div` 2


--- SAVING TREE
-- Transform tree into string
tree2string level (Leaf cls) = replicate (level*2) ' ' ++ "Leaf: " ++ cls ++ "\n"
tree2string level (Node idx threshold ltree rtree) =
    replicate (level*2) ' ' ++ "Node: " ++ show idx ++ ", " ++ show threshold ++ "\n" ++ tree2string (level+1) ltree ++ tree2string (level+1) rtree


--- TRAINING
-- Train tree
-- For each column:
    -- 1. sort column                                                       check
    -- 2. get average intermediate values (between values)                  check
    -- 3. calculate Gini impurity for each of them - more calculations      check
        -- a. for both branches -> 1 - probability of all classes squared   check
        -- b. total gini impurity -> weighted average from two branches     check
        -- c. select threshold with the lowest gini impurity                check
    -- 4. select column (feature) with the lowest gini impurity             check

-- Split data:
        -- 1. Get under threshold and over threshold
        -- 2. Delete the winner column? Or skip it? Remember, the column index will be needed
            -- Keep list of indexes. On delete, delete also from the list of indexes.
            -- New column index will point to the true index in the original list
    -- Recursively call for each branch
    -- Count classes in the branch
        -- Get under threshold or over threshold and call isPure'
        -- Or after split call isPure
    -- If the branch is pure, return Leaf with the class
    -- How to find out if the branch is pure? -> Check if all targets are the same
    -- If max depth is reached, return Leaf with the class that has majority
        -- Max depth = columns_count (in recursive call, decrease the depth)
    -- If the branch is not pure, return Node with the feature index and threshold
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


-- Obtain column of features with index i
getColumn i = map (!! i)

-- Get average values for adjacent points in one column
getIntermediateValues [] = []
getIntermediateValues [x] = []
getIntermediateValues (x:y:xs) = (x + y) / 2.0 : getIntermediateValues (y:xs)

initCounter classes = replicate (length classes) 0

-- TODO: called upon empty list -> FIX!!!!
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

-- Get weighted Gini impurities for one column
getWeightedGinis targets = zipWith weightedGiniImpurity under_threshold over_threshold
    where
        -- Get under and over threshold counts
        under_threshold = getUnderThresholdTargets targets unique_targets
        over_threshold = reverse $ getUnderThresholdTargets (reverse targets) unique_targets

        -- List of unique targets
        unique_targets = sort $ nub targets

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