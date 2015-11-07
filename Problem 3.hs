isqrt 0 = 0
isqrt 1 = 1
isqrt n = head $ dropWhile (\x -> x*x > n) $ iterate (\x -> (x + n `div` x) `div` 2) (n `div` 2)

factors :: Integral (a) => a -> [a]
factors number = lowerFactors ++ if ((head upperFactors)^2 == number) then (tail upperFactors) else upperFactors
	where	lowerFactors = filter ((==) 0 . rem number) [1..isqrt number]
		upperFactors = reverse (map (div number) lowerFactors)

primeFactors :: Integral (a) => a -> [a]
primeFactors number = filter isPrime (factors number)

isPrime :: Integral (a) => a -> Bool
isPrime 1 = False
isPrime realNumber = all ((/=) 0 . mod number) [2..isqrt number]
	where number = abs realNumber

main = print $ maximum $ primeFactors 600851475143