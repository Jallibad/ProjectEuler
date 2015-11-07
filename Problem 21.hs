isqrt :: Integral (a) => a -> a
isqrt 0 = 0
isqrt 1 = 1
isqrt n = head $ dropWhile (\x -> x*x > n) $ iterate (\x -> (x + n `div` x) `div` 2) (n `div` 2)

factors :: Integral (a) => a -> [a]
factors number = lowerFactors ++ if ((head upperFactors)^2 == number) then (tail upperFactors) else upperFactors
	where	lowerFactors = filter ((==) 0 . rem number) [1..(isqrt number)]
		upperFactors = reverse (map (div number) lowerFactors)

amicable :: Integral (a) => a -> a
amicable 0 = 0
amicable number = sum $ init $ factors number

isAmicable :: Integral (a) => a -> Bool
isAmicable a = a == amicable b && a /= b
	where b = amicable a

main = print $ sum $ filter isAmicable [1..pred 10000]