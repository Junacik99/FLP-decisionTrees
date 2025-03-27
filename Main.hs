------------------------------
-- Decision Tree Classifier --
--           FLP            --
--      Martin Takacs       --
--         xtakac07         --
--           2025           --
------------------------------

import System.Environment (getArgs)
import Training ( training )
import Classification ( classification )

parseArgs :: [String] -> IO ()
parseArgs [] = error "No arguments were provided"
parseArgs ("-h":_) = do putStrLn "Help"
parseArgs ("-1":xs) = classification (head xs) (head $ tail xs)
parseArgs ("-2":xs) = training $ head xs
parseArgs (x:_) = error $ "Unknown argument: " ++ x

main :: IO ()
main = do
    args <- getArgs
    parseArgs args
