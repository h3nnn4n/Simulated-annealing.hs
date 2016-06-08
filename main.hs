import Data.List
import Data.List.Split
import System.Environment
import System.IO
import System.Random
import Control.Monad

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

randomVars :: Int -> StdGen -> [Int]
randomVars n = take n . unfoldr (Just . randomR (0,1))

main = do
    args  <- getArgs
    pName <- getProgName

    when (null args) $ error $ "Usage: " ++ pName ++  " file.cnf"

    file  <- readFile (head args)
    g     <- newStdGen

    let formula = parseFile file
        size    = ( \(x, _, _) -> x ) (head formula)
        s0      = randomVars size g
        (x, y)  = randomR (0, 1 :: Int) g

    print s0
    print formula

    return ()
