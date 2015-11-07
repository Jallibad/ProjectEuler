import Data.List

isqrt :: Integral (a) => a -> a
isqrt 0 = 0
isqrt 1 = 1
isqrt n = head $ dropWhile (\x -> x*x > n) $ iterate (\x -> (x + n `div` x) `div` 2) (n `div` 2)

isPrime :: Integral (a) => a -> Bool
isPrime 1 = False
isPrime number = all ((/=) 0 . mod number) [2..isqrt number]

isCircularPrime :: Integral (a) => Show (a) => a -> Bool
isCircularPrime number = (length string == 1 || invalids \\ string == invalids) && (all (isPrime . read) $ circulateList string)
	where	invalids = "024568"
		string = show number

circulateList :: [a] -> [[a]]
circulateList list = tail $ zipWith (++) (tails list) (inits list)

main = print $ length $ filter isCircularPrime [1..10^6]