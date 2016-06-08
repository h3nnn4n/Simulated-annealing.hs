module Random where

import Data.List
import System.Random

randomVars :: Int -> StdGen -> [Int]
randomVars n = take n . unfoldr (Just . randomR (0,1))
