module Decisiontree
(
    predict,
    predictTree,
    loadTree,
    tree2string
)
where
---- DECISION TREE ----
-- This module contains all functionality of the decision tree:
--      Traversing
--      Predictions
--      TODO: Training
import Datatypes
import Utils

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
-- TODO