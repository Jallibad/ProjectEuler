import Data.Function

--NOT MINE - taken from "http://math.stackexchange.com/questions/736008/understanding-the-partition-function"
p :: Int -> Int -> Int
p m n
	| n < 0  = 0
	| n == 0 = 1
	| otherwise = sum [p i (n - i) | i <- [m..n]]

memoize :: (Int -> a) -> (Int -> a)
memoize f = (map f [0 ..] !!)

partitionMemo = fix (memoize . p)