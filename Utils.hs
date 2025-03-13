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
import Distribution.Compat.Prelude (readMaybe)
import Text.Read (readEither)
--- UTILS ---
-- This file contains all utility functions


-- Split string on element e
splitOn e = foldr f [[]]
    where
        f x (acc:rest)
            | x == e = []:(acc:rest)
            | otherwise = (x:acc) : rest

-- Split separate lines into features
splitLines _ [] = []
splitLines e (x:xs) = splitOn e x : splitLines e xs

-- Convert nodes into Float
convertData [] = []
convertData (x:xs) = convert x : convertData xs
    where
        convert [] = []
        convert (x:xs) = (read x :: Float) : convert xs

-- Count number of Chars in a sequence in String (from beginning)
countChar _ "" = 0
countChar x xs = if x == head xs then 1 + countChar x (tail xs) else 0

-- Trim lines from leading elements
trimLines _ [] = []
trimLines e (x:xs) = (dropWhile (== e) (fst x), snd x) : trimLines e xs

-- dropWhile applied to the snd element of a tuple in a list
dropWhileSecond condition = dropWhile (\(_, y) -> condition y)

-- Delete element at index n
deleteAt _ [] = []
deleteAt 0 (x:xs) = xs
deleteAt n (x:xs) = x : deleteAt (n - 1) xs

-- Get value from Maybe
safeGet (Just x) = x
safeGet Nothing = error "No value"

-- Get index of the minimum element in the list
minIndex [] = 0
minIndex xs = head $ filter (\i -> xs !! i == minimum xs) [0..length xs - 1]