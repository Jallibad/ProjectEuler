isqrt :: Integral (a) => a -> a
isqrt 0 = 0
isqrt 1 = 1
isqrt n = head $ dropWhile (\x -> x*x > n) $ iterate (\x -> (x + n `div` x) `div` 2) (n `div` 2)

isPrime :: Integral (a) => a -> Bool
isPrime 1 = False
isPrime number = all ((/=) 0 . mod number) [2..isqrt number]

primes = filter isPrime [1..]

primeSums :: Integer -> Int
primeSums original = primeSums' original original
	where
		primeSums' 0 max = 1
		primeSums' 1 max = 0
		primeSums' number max = sum $ map (\prime -> primeSums' (number-prime) prime) $ takeWhile (<=largestPossible) primes
			where largestPossible = minimum [number, max]

main = print $ until (\x -> primeSums x > 5000) (+1) 1