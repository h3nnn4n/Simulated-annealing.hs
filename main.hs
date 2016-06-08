import System.Environment
import System.IO
import Control.Monad
import System.Random

import File
import Random

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
