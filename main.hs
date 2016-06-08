import System.Environment
import System.IO
import Data.List
import Data.List.Split

magic []     = []
magic (x:xs)
    | head x == 'c' = magic xs
    | head x == 'p' = magic xs
    -- | x == "%"      = []
    -- | elem '%' x    = []
    | otherwise     = map ( \x -> (read x :: Int)) $ filter ( /= " ") (splitOn " " x) ++ magic xs

parseFile file = do
    let todo = lines file
    --print todo
    print $ magic todo
    return todo

main = do
    args <- getArgs
    file <- readFile (head args)

    formula <- parseFile file
    --print formula

    return ()
