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
    g2    <- newStdGen
    g3    <- newStdGen

    let formula' = parseFile file
        size     = ( \(x, _, _) -> x ) (head formula')
        formula  = tail formula'
        s0       = randomVars size g1
        --s0       = [True,True,False,False,True,False,True,True,False,True,True,False,False,False,False,False,True,True,True,False]
        changes  = randomRs (0, 1      :: Double ) g2
        pos      = randomRs (0, size-1 :: Int    ) g2
        maxIters = 10000

    --print formula
    print $ s0
    print $ loop s0 formula pos changes maxIters maxIters
    print $ objFunc    s0 formula
    print $ objFunc ( loop s0 formula pos changes maxIters maxIters ) formula

    return ()
