{-# LANGUAGE ViewPatterns, TupleSections #-}

module MathFunctions where

import ListFunctions
import Sieve

import Data.Array
import Data.Bits
import Data.Char (digitToInt)
import Data.List
import Data.Ratio

isqrt :: Integral a => a -> a
isqrt 0 = 0
isqrt 1 = 1
isqrt n = head $ dropWhile (\x -> x*x > n) $ iterate (\x -> (x + n `div` x) `div` 2) (n `div` 2)

amicable :: Integral a => a -> a
amicable 0 = 0
amicable n = sum $ init $ factors n

isAmicable :: Integral a => a -> Bool
isAmicable a = a == amicable b && a /= b
	where b = amicable a

isPrime :: Integral a => a -> Bool
isPrime ((`elem` [-1,0,1]) -> True) = False
isPrime (abs -> n) = all ((/=) 0 . mod n) [2..isqrt n]

isCircularPrime :: (Integral a, Show a) => a -> Bool
isCircularPrime number = (length string == 1 || invalids \\ string == invalids) && (all (isPrime . read) $ circulateList string)
	where	invalids = "024568"
		string = show number

isTruncatablePrime :: (Integral a, Show a) => a -> Bool
isTruncatablePrime n = (n >= 10) && (all (isPrime . read) $ (tail $ inits s)++(init $ tails s))
	where s = show n

primes :: Integral a => [a]
primes = 2 : filter isPrime [3,5..]

isSquare n = (isqrt n)^2 == n

factors :: Integral a => a -> [a]
factors number = lowerFactors ++ if isSquare number then (tail upperFactors) else upperFactors
	where	lowerFactors = filter ((==0) . rem number) [1..isqrt number]
		upperFactors = reverse (map (div number) lowerFactors)

primeFactors :: Integral a => a -> [a]
primeFactors n = reverse $ fst $ until ((==1) . snd) (\(p, x) -> let f = head $ dropWhile ((/=0) . (x `mod`)) primes in (f:p, x `div` f)) ([], n)

distinctPrimeFactors = nub . primeFactors

factorization :: Integral a => a -> [a]
factorization =	unfoldr foldFunction
	where
		foldFunction 1 = Nothing
		foldFunction x = let firstPrime = head $ dropWhile (\n -> x `mod` n /= 0) primes in
					Just (firstPrime, x `div` firstPrime)

factorial :: Integral a => a -> a
factorial 0 = 1
factorial n = product [1..n]

factorials :: Integral a => [a]
factorials = 1 : scanl1 (*) [1..]

digitFactorial :: (Integral a, Show a) => a -> Int
digitFactorial = sum . map ((factorials !!) . digitToInt) . show

combinatoric :: Integral (a) => a -> a -> a
combinatoric n k = truncate $ product $ [(n+1-i) % i | i <- [1.. min k $ n-k]]

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

intToBinary :: (Integral a, Bits a) => a -> [Bool]
intToBinary n = map (testBit n) [0..truncate $ logBase 2 $ fromIntegral n]

primitivePythagoreanTriples = [(a,b,c) | m <- [1..], n <- [(ceiling $ (sqrt 2)*(fromIntegral $ abs m))-m..m-1], even m || even n, gcd m n == 1, let a=m^2-n^2, let b=2*m*n, let c=m^2+n^2]