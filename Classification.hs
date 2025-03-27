------------------------------
-- Decision Tree Classifier --
--           FLP            --
--      Martin Takacs       --
--         xtakac07         --
--           2025           --
------------------------------
module Classification (
    classification
) where

import Datatypes ()
import Decisiontree ( predictTree, loadTree )
import Utils ( convertData, splitLines )

-- Loading new data and trained tree + classification
classification :: FilePath -> FilePath -> IO ()
classification tree_path data_path = do

    -- Obtain data from file to predict 
    -- print "Loading data points"
    content <- readFile data_path
    let test_data = convertData $ splitLines ',' $ lines content
    -- print test_data

    -- Obtain tree from file
    -- print "Loading tree"
    content_tree <- readFile tree_path
    let lines_tree = lines content_tree
    let tree = loadTree lines_tree
    -- print tree

    -- Predict (decide)
    -- print "Calculating predictions"
    let predictions = predictTree tree test_data
    -- Print predictions in lines (without last new line)
    putStrLn (init $ unlines predictions)