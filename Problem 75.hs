import qualified Data.Map.Strict as Map

main = print $ Map.size $ Map.filter (==1) $ Map.fromListWith (+) $ zip (concatMap (\x -> [x, 2*x..15*10^5]) $ [2*m*(m+n) | m <- [1..875], n <- [1..m], gcd m n == 1, even m || even n]) $ repeat 1