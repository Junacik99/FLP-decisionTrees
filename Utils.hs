module Utils
(
    splitOn,
    splitLines,
    convertData,
    countChar,
    trimLines,
    dropWhileSecond
) where
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

trimLines _ [] = []
trimLines e (x:xs) = (dropWhile (== e) (fst x), snd x) : trimLines e xs

dropWhileSecond condition = dropWhile (\(_, y) -> condition y)