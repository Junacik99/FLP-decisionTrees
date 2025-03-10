import Datatypes
import Decisiontree
import Utils
import Data.List (sort, nub, elemIndex, sortBy, minimumBy)
-- import Data.Foldable (minimumBy)

-- Obtain column of features with index i
getColumn i = map (!! i)

-- Get average values for adjacent points in one column
getIntermediateValues [] = []
getIntermediateValues [x] = []
getIntermediateValues (x:y:xs) = (x + y) / 2.0 : getIntermediateValues (y:xs)

initCounter classes = replicate (length classes) 0

-- Get number of targets under threshold
getUnderThreshold targets classes = init $ tail $ scanl updateCounter (initCounter classes) targets
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
getCandidateThreshold col_idx features targets = intermediate_values !! minIndex weighted_gini
    where
        -- Get intermediate values for one column
        intermediate_values = getIntermediateValues $ getColumn col_idx features

        -- Calculate weighted Gini impurity for each threshold
        weighted_gini = zipWith weightedGiniImpurity under_threshold over_threshold

        -- Get under and over threshold counts
        under_threshold = getUnderThreshold targets unique_targets
        over_threshold = reverse $ getUnderThreshold (reverse targets) unique_targets

        -- List of unique targets
        unique_targets = sort $ nub targets


getWeightedGinis targets = zipWith weightedGiniImpurity under_threshold over_threshold
    where
        -- Get under and over threshold counts
        under_threshold = getUnderThreshold targets unique_targets
        over_threshold = reverse $ getUnderThreshold (reverse targets) unique_targets

        -- List of unique targets
        unique_targets = sort $ nub targets

getMinGinis features targets = map ( $ sort) features
    where
        -- Get under and over threshold counts
        under_threshold = getUnderThreshold targets unique_targets
        over_threshold = reverse $ getUnderThreshold (reverse targets) unique_targets

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
getNode (Just feature_idx, (_, threshold_idx)) features = (feature_idx, threshold)
    -- This is the threshold that will be used as a root node (snd (snd winner) is the feature index)
    where threshold = getIntermediateValues (getColumn feature_idx features) !! threshold_idx
getNode (Nothing, _) _ = error "No Node"

-- Load train data + train tree
training :: IO ()
training = do

    -- let tree_test = Node 0 5.5 (Leaf "TridaA") (Node 1 3.0 (Node 2 2.0 (Leaf "TridaA") (Leaf "TridaC")) (Node 2 3.5 (Leaf "TridaB") (Leaf "TridaC")))

    -- Load data
    content <- readFile "test/data/data_table.csv"
    let loaded_data = splitLines ',' $ lines content            -- Raw data
    let train_features = convertData $ map init loaded_data     -- Features
    let train_targets = map last loaded_data                    -- Targets
    let train_data = zip train_features train_targets           -- Data (features + targets)
    let unique_targets = sort $ nub train_targets               -- List of unique targets
    -- print unique_targets
    -- print train_features
    -- print train_targets

    -- TODO: Train tree
    -- For each column:
    -- 1. sort column                                                       TODO
    -- 2. get average intermediate values (between values)                  check
    -- 3. calculate Gini impurity for each of them - more calculations      TODO
        -- a. for both branches -> 1 - probability of all classes squared   check
        -- b. total gini impurity -> weighted average from two branches     check
        -- c. select threshold with the lowest gini impurity                check
    -- 4. select column (feature) with the lowest gini impurity             TODO

    -- Get ONE column
    -- let column = getColumn 0 train_features

    -- -- Sort targets by feature column
    -- let sorted_data = sortBy (\x y -> compare (fst x) (fst y)) $ zip column train_targets
    -- let sorted_features = map fst sorted_data
    -- let sorted_targets = map snd sorted_data

    -- -- Get weighted Gini impurities and index of the lowest value
    -- let weightedGinis = getWeightedGinis sorted_targets
    -- let gini_idx = (minimum weightedGinis, minIndex weightedGinis)

    -- Get min Gini impurity and feature index for each column
    let min_ginis = map (\n -> getGiniIdx train_targets $ getColumn n train_features) [0..length (head train_features) - 1]

    -- Get the winner node (feature index, threshold)
    let winner = (elemIndex (findMinTuple min_ginis) min_ginis, findMinTuple min_ginis)
    let winner_node = getNode winner train_features
    print winner_node

    -- let candidate_threshold = getCandidateThreshold 2 train_features train_targets
    -- print candidate_threshold

    -- TODO: dont return threshold, but the gini impurity and the threshold index

    -- Create string from tree and delete last empty line
    -- let tree_to_save = init $ tree2string 0 tree_test
    -- print tree_to_save
    -- writeFile "output.txt" tree_to_save

    print ""