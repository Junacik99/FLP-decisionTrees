module Classification (
    classification
) where

import Datatypes
import Decisiontree
import Utils

-- Loading new data and trained tree + classification
classification data_path tree_path = do


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