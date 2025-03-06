import Datatypes
import Decisiontree
import Utils


-- Load train data + train tree
training :: IO ()
training = do

    let tree_test = Node 0 5.5 (Leaf "TridaA") (Node 1 3.0 (Node 2 2.0 (Leaf "TridaA") (Leaf "TridaC")) (Node 2 3.5 (Leaf "TridaB") (Leaf "TridaC")))

    -- Create string from tree and delete last empty line
    let tree_to_save = init $ tree2string 0 tree_test
    print tree_to_save
    writeFile "output.txt" tree_to_save