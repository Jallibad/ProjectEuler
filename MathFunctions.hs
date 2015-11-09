module MathFunctions where

import Data.List
import Data.Char (digitToInt)

isqrt :: Integral (a) => a -> a
isqrt 0 = 0
isqrt 1 = 1
isqrt n = head $ dropWhile (\x -> x*x > n) $ iterate (\x -> (x + n `div` x) `div` 2) (n `div` 2)

isPrime :: Integral (a) => a -> Bool
isPrime realNumber = case abs realNumber of
				0 -> False
				1 -> False
				number -> all ((/=) 0 . mod number) [2..isqrt number]

primes :: [Integer]
primes = filter isPrime [1..]

isSquare n = (isqrt n)^2 == n

factors :: Integral (a) => a -> [a]
factors number = lowerFactors ++ if isSquare number then (tail upperFactors) else upperFactors
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

factorial :: Integral (a) => a -> a
factorial 0 = 1
factorial n = product [1..n]

factorials = 1 : 1 : scanl1 (*) [2..]

digitFactorial :: Integral (a) => Show (a) => a -> Int
digitFactorial number = sum $ map (\x -> factorials !! digitToInt x) $ show number

combinatoric :: Integral (a) => a -> a -> a
combinatoric n r = (factorial n) `div` ((factorial (n-r))*(factorial r))

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)