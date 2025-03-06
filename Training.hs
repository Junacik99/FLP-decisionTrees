import Datatypes
import Decisiontree
import Utils
import Data.List (sort)

-- Obtain column of features with index i
getColumn i = map (!! i)

-- Get average values for adjacent points
getIntermediateValues [] = []
getIntermediateValues [x] = []
getIntermediateValues (x:y:xs) = (x + y) / 2.0 : getIntermediateValues (y:xs)

-- Load train data + train tree
training :: IO ()
training = do

    let tree_test = Node 0 5.5 (Leaf "TridaA") (Node 1 3.0 (Node 2 2.0 (Leaf "TridaA") (Leaf "TridaC")) (Node 2 3.5 (Leaf "TridaB") (Leaf "TridaC")))

    -- Load data
    content <- readFile "test/data/data_table.csv"
    let train_data = splitLines ',' $ lines content
    let train_features = convertData $ map init train_data
    let train_targets = map last train_data
    -- print train_features
    -- print train_targets


    -- TODO: Train tree
    print $ getColumn 1 train_features

    -- 1. sort column                                                       check
    -- 2. get average intermediate values (between values)                  check
    -- 3. calculate Gini impurity for each of them - more calculations      TODO


    -- Create string from tree and delete last empty line
    -- let tree_to_save = init $ tree2string 0 tree_test
    -- print tree_to_save
    -- writeFile "output.txt" tree_to_save

    print ""