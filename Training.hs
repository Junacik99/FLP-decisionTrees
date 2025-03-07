import Datatypes
import Decisiontree
import Utils
import Data.List (sort, nub, elemIndex)

-- Obtain column of features with index i
getColumn i = map (!! i)

-- Get average values for adjacent points
getIntermediateValues [] = []
getIntermediateValues [x] = []
getIntermediateValues (x:y:xs) = (x + y) / 2.0 : getIntermediateValues (y:xs)

initCounter classes = replicate (length classes) 0

getUnderThreshold targets classes = init $ tail $ scanl updateCounter (initCounter classes) targets
    where updateCounter counter target =
            case elemIndex target classes of
                Just idx -> take idx counter ++ [counter !! idx + 1] ++ drop (idx + 1) counter
                Nothing  -> counter  -- If the target is not in the classes list, leave the counter unchanged


-- Load train data + train tree
training :: IO ()
training = do

    -- let tree_test = Node 0 5.5 (Leaf "TridaA") (Node 1 3.0 (Node 2 2.0 (Leaf "TridaA") (Leaf "TridaC")) (Node 2 3.5 (Leaf "TridaB") (Leaf "TridaC")))

    -- Load data
    content <- readFile "test/data/data_table.csv"
    let loaded_data = splitLines ',' $ lines content
    let train_features = convertData $ map init loaded_data
    let train_targets = map last loaded_data
    let train_data = zip train_features train_targets
    let unique_targets = sort $ nub train_targets
    -- print unique_targets
    -- print train_features
    -- print train_targets


    -- TODO: Train tree

    -- 1. sort column                                                       check
    -- 2. get average intermediate values (between values)                  check
    -- 3. calculate Gini impurity for each of them - more calculations      TODO
        -- a. for both branches -> 1 - probability of all classes squared
        -- b. total gini impurity -> weighted average from two branches
        -- c. select threshold with the lowest gini impurity
    -- 4. select column (feature) with the lowest gini impurity

    print $ getUnderThreshold train_targets unique_targets


    -- Create string from tree and delete last empty line
    -- let tree_to_save = init $ tree2string 0 tree_test
    -- print tree_to_save
    -- writeFile "output.txt" tree_to_save

    print ""