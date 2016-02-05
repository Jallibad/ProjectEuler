import MathFunctions

primeSums :: Integer -> Int
primeSums original = primeSums' original original
	where
		primeSums' 0 _ = 1
		primeSums' 1 _ = 0
		primeSums' number max = sum $ map (\prime -> primeSums' (number-prime) prime) $ takeWhile (<=largestPossible) primes
			where largestPossible = minimum [number, max]

main = print $ until ((>5000) . primeSums) (+1) 1