import Datatypes
import Decisiontree
import Utils

main :: IO ()
main = do


    -- Obtain data from file to predict 
    content <- readFile "test/data/test2.txt"
    let lines = splitLines ',' $ splitOn '\n' content
    let newdata = convertData lines
    print newdata

    -- Obtain tree from file
    content_tree <- readFile "test/trees/treetest2.txt"
    let lines_tree = splitOn '\n' content_tree
    let tree = loadTree lines_tree
    print tree


    -- Create test tree
    -- let tree_test = Node 0 5.5 (Leaf "TridaA") (Node 1 3.0 (Leaf "TridaB") (Leaf "TridaC"))
    -- print tree_test

    -- Predict (decide)
    let predictions = predictTree tree newdata
    print predictions