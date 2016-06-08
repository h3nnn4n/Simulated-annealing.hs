module Cnf where

applyClausule _    []     = []
applyClausule vars (e:es) = ( x1 || x2 || x3 ) : applyClausule vars es
    where
        (a, b, c) = e
        x1 = if a > 0 then ( vars !! (a-1) ) else ( not ( vars !! ( (-a)-1 )))
        x2 = if b > 0 then ( vars !! (b-1) ) else ( not ( vars !! ( (-b)-1 )))
        x3 = if c > 0 then ( vars !! (c-1) ) else ( not ( vars !! ( (-c)-1 )))

objFunc vars formula = 1.0 - ( fromIntegral q ) / ( fromIntegral p )
    where
        q = length ( filter ( == True ) ( applyClausule vars formula ))
        p = length ( applyClausule vars formula )

loop :: [Bool] -> [(Int, Int, Int)] -> [Int] -> [Double] -> Int -> Int -> [Bool]
loop vars _       _      _      0   _ = vars
loop vars formula (p:ps) (c:cs) acc m = loop e2 formula ps cs (acc-1) m
    where
        e'              = first ++ [not (vars !! p)] ++ second
        e               = vars
        f               = objFunc e  formula
        f'              = objFunc e' formula
        (a, b)          = splitAt p vars
        t               = temp acc m
        (first, second) = if   length a > 0
                          then (init a,      b)
                          else (     a, tail b)
        e2              = if f' < f
                          then e'
                          else if exp ( -(f' - f) / t ) > c then e' else e

temp :: Int -> Int -> Double
temp acc maxIters = t_0 * (( t_n / t_0) ** (n / ( fromIntegral maxIters)))
    where
        t_0 = 100.0
        t_n = 0.0
        n   = fromIntegral (maxIters - acc)
