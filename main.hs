import System.Environment
import System.IO
import Control.Monad
import System.Random

import File
import Random
import Cnf

main = do
    args  <- getArgs
    pName <- getProgName

    when (null args) $ error $ "Usage: " ++ pName ++  " file.cnf"

    file  <- readFile (head args)
    g1    <- newStdGen
    --g2    <- newStdGen
    --g3    <- newStdGen

    let formula  = parseFile file
        size     = ( \(x, _, _) -> x ) (head formula)
        s0       = randomVars size g1
        --(x, y)   = randomR (0, 1 :: Int) g1
        changes  = randomRs (0, 1      :: Double ) g1
        pos      = randomRs (0, size-1 :: Int ) g1
        maxIters = 10000

    print $ s0
    print $ loop s0 formula pos changes maxIters maxIters
    print $ length $ s0
    print $ length $ loop s0 formula pos changes maxIters maxIters
    --print $ applyClausule (s0 ++ s0) (tail formula)

    return ()
