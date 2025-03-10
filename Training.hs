import Datatypes
import Decisiontree
import Utils
import Data.List (sort, nub, elemIndex)

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

    -- 1. sort column                                                       check
    -- 2. get average intermediate values (between values)                  check
    -- 3. calculate Gini impurity for each of them - more calculations      TODO
        -- a. for both branches -> 1 - probability of all classes squared   check
        -- b. total gini impurity -> weighted average from two branches     check
        -- c. select threshold with the lowest gini impurity                TODO
    -- 4. select column (feature) with the lowest gini impurity

    -- Get under and over threshold counts
    let under_threshold = getUnderThreshold train_targets unique_targets
    let over_threshold = reverse $ getUnderThreshold (reverse train_targets) unique_targets

    -- Get intermediate values
    let intermediate_values = getIntermediateValues $ getColumn 2 train_features

    -- Calculate Gini impurity for each threshold
    -- let gini_under = map giniImpurity under_threshold
    -- let gini_over = map giniImpurity over_threshold
    -- print gini_under
    -- print gini_over

    -- Calculate weighted Gini impurity for each threshold
    let weighted_gini = zipWith weightedGiniImpurity under_threshold over_threshold
    print weighted_gini

    -- Create string from tree and delete last empty line
    -- let tree_to_save = init $ tree2string 0 tree_test
    -- print tree_to_save
    -- writeFile "output.txt" tree_to_save

    print ""