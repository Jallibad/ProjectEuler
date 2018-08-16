{-# LANGUAGE TupleSections #-}

import qualified Data.Map.Strict as Map
import MathFunctions

bigThing upperBound = map (,1) $ concatMap (\x -> [2*x, 4*x..upperBound]) [m*(m+n) | m <- [1..875], n <- [1..m], gcd m n == 1, even m || even n]

main = print $ Map.size $ Map.filter (==1) $ Map.fromListWith (+) $ bigThing $ 15*10^5