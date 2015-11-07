module MathFunctions where

import Data.List
import Data.Char (digitToInt)

isqrt :: Integral (a) => a -> a
isqrt 0 = 0
isqrt 1 = 1
isqrt n = head $ dropWhile (\x -> x*x > n) $ iterate (\x -> (x + n `div` x) `div` 2) (n `div` 2)

isPrime :: Integral (a) => a -> Bool
isPrime 1 = False
isPrime (-1) = False
isPrime 0 = False
isPrime realNumber = all ((/=) 0 . mod number) [2..isqrt number]
	where number = abs realNumber

primes :: [Integer]
primes = filter isPrime [1..]

factors :: Integral (a) => a -> [a]
factors number = lowerFactors ++ if ((head upperFactors)^2 == number) then (tail upperFactors) else upperFactors
	where	lowerFactors = filter ((==0) . rem number) [1..isqrt number]
		upperFactors = reverse (map (div number) lowerFactors)

primeFactors :: Integral (a) => a -> [a]
primeFactors number = filter isPrime (factors number)

factorization :: Integer -> [Integer]
factorization =	unfoldr (\x ->	if x==1 then
					Nothing
				else
					let firstPrime = head $ dropWhile (\n -> x `mod` n /= 0) primes in
					Just (firstPrime, x `div` firstPrime)
			)

factorials = 1 : 1 : scanl1 (*) [2..]

digitFactorial :: Integral (a) => Show (a) => a -> Int
digitFactorial number = sum $ map (\x -> factorials !! digitToInt x) $ show number