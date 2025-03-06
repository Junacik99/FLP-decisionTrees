module Datatypes
(
    Tree (..),
    Index,
    Feature,
    Class
)
where
---- DATA TYPES ----
-- This module contains custom data types

-- Alias types for better readability
type Index = Int
type Feature = Float
type Class = String

-- Tree can be either node or leaf
-- Node has:
--  Feature index - Int
--  Threshold value - Float
--  Left subtree - Tree
--  Right subtree - Tree
-- Leaf has:
--  Class - String
data Tree = Node Index Feature Tree Tree | Leaf Class deriving Show