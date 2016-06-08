module Random where

import Data.List
import System.Random

randomVars :: Int -> StdGen -> [Bool]
randomVars n g = map magic ((take n . unfoldr (Just . randomR (0, 1))) g)

magic :: Int -> Bool
magic 1 = True
magic 0 = False
