isqrt 0 = 0
isqrt 1 = 1
isqrt n = head $ dropWhile (\x -> x*x > n) $ iterate (\x -> (x + n `div` x) `div` 2) (n `div` 2)

factors :: Integral (a) => a -> [a]
factors number = lowerFactors ++ if ((head upperFactors)^2 == number) then (tail upperFactors) else upperFactors
	where	lowerFactors = filter ((==) 0 . rem number) [1..isqrt number]
		upperFactors = reverse $ map (div number) lowerFactors

isPrime :: Integral (a) => a -> Bool
isPrime 1 = False
isPrime realNumber = all ((/=) 0 . mod number) [2..isqrt number]
	where number = abs realNumber

primeFactorLengths = map (length . filter isPrime . factors) [1..]

distinctPrimeFactors :: Int -> Int
distinctPrimeFactors d = until (\x -> all (==d) $ take d $ drop (x-1) primeFactorLengths) (+1) 1

main = print $ distinctPrimeFactors 4