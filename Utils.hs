{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use first" #-}
module Utils
(
    splitOn,
    splitLines,
    convertData,
    countChar,
    trimLines,
    dropWhileSecond,
    deleteAt,
    safeGet,
    minIndex
) where
--- UTILS ---
-- This file contains all utility functions


-- Split string on element e
splitOn :: (Foldable t, Eq a) => a -> t a -> [[a]]
splitOn e = foldr f [[]]
    where
        f _ [] = [[]]
        f x (acc:rest)
            | x == e = []:(acc:rest)
            | otherwise = (x:acc) : rest

-- Split separate lines into features
splitLines :: (Foldable t1, Eq t2) => t2 -> [t1 t2] -> [[[t2]]]
splitLines _ [] = []
splitLines e (x:xs) = splitOn e x : splitLines e xs

-- Convert nodes into Float
convertData :: [[String]] -> [[Float]]
convertData [] = []
convertData (x:xs) = convert x : convertData xs
    where
        convert [] = []
        convert (y:ys) = (read y :: Float) : convert ys

-- Count number of Chars in a sequence in String (from beginning)
countChar :: Num a => Char -> [Char] -> a
countChar _ "" = 0
countChar x xs = if x == head xs then 1 + countChar x (tail xs) else 0

-- Trim lines from leading elements
trimLines :: Eq t => t -> [([t], b)] -> [([t], b)]
trimLines _ [] = []
trimLines e (x:xs) = (dropWhile (== e) (fst x), snd x) : trimLines e xs

-- dropWhile applied to the snd element of a tuple in a list
dropWhileSecond :: (t -> Bool) -> [(a, t)] -> [(a, t)]
dropWhileSecond condition = dropWhile (\(_, y) -> condition y)

-- Delete element at index n
deleteAt :: (Eq t, Num t) => t -> [a] -> [a]
deleteAt _ [] = []
deleteAt 0 (_:xs) = xs
deleteAt n (x:xs) = x : deleteAt (n - 1) xs

-- Get value from Maybe
safeGet :: Maybe a -> a
safeGet (Just x) = x
safeGet Nothing = error "No value"

-- Get index of the minimum element in the list
minIndex :: Ord a => [a] -> Int
minIndex [] = 0
minIndex xs = head $ filter (\i -> xs !! i == minimum xs) [0..length xs - 1]