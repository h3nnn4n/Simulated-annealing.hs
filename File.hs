module File where

import Data.List.Split

magic []     = []
magic (x:xs)
    | head x == 'c' = magic xs
    | head x == 'p' = header : magic xs
    | x      == "%" = []
    | otherwise     = ret : magic xs
        where
            ret     = (a!!0, a!!1, a!!2)
            a       = map ( \x -> (read x :: Int)) ( filter ( /= "" ) (splitOn " " x          ))
            header' = map ( \x -> (read x :: Int)) ( filter ( /= "" ) (splitOn " " (drop 5 x )))
            header  = (header'!!0, header'!!1, 0)

parseFile file = magic ( lines file )
