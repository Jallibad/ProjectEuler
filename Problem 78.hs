import Data.Function
import Data.List (genericIndex)

--NOT MINE - taken from "http://math.stackexchange.com/questions/736008/understanding-the-partition-function"
p :: ((Integer, Integer) -> Integer) -> (Integer, Integer) -> Integer
p recurse (m,n)
	| n < 0  = 0
	| n == 0 = 1
	| otherwise = sum [recurse (i, (n - i)) | i <- [m..n]]

partitions :: [[Integer]]
partitions = map (\n -> map (\m -> p partition (m,n)) [1..n]) [1..]

partition :: (Integer, Integer) -> Integer
partition (m, n) = (partitions `genericIndex` n) `genericIndex` m