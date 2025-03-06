import Datatypes
import Decisiontree
import Utils

main :: IO ()
main = do


    -- Obtain data from file to predict 
    print "Loading data points"
    content <- readFile "test/data/test2.txt"
    let lines = splitLines ',' $ splitOn '\n' content
    let newdata = convertData lines
    print newdata

    -- Obtain tree from file
    print "Loading tree"
    content_tree <- readFile "test/trees/treetest2.txt"
    let lines_tree = splitOn '\n' content_tree
    let tree = loadTree lines_tree
    print tree

    -- Predict (decide)
    print "Calculating predictions"
    let predictions = predictTree tree newdata
    print predictions