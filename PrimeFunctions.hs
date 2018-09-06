{-# LANGUAGE ViewPatterns #-}

module PrimeFunctions where

import Data.List (intersect, tails)
import ListFunctions (circulateList)
import MathFunctions hiding (isPrime)
import System.Random (getStdGen, Random, randomRs)

primes :: Integral a => [a]
primes = 2 : 3 : filter isPrime (concatMap (\k -> [6*k-1,6*k+1]) [1..])

isPrime :: Integral a => a -> Bool
isPrime ((`elem` [-1,0,1]) -> True) = False
isPrime (abs -> n) = all ((/=) 0 . mod n) [2..isqrt n]

isCircularPrime :: Integral a => a -> Bool
isCircularPrime n
	| n < 10 = isPrime n
	| otherwise = null (intersect invalids s) && (all (isPrime . digitsList 10) $ circulateList s)
	where
		invalids = [0,2,4,5,6,8]
		s = digits n

isTruncatablePrime :: Integral a => a -> Bool
isTruncatablePrime = andAll [(>= 10), left, right]
	where
		left = all isPrime . takeWhile (/=0) . iterate (`div` 10)
		right = all (isPrime . digitsList 10) . init . tails . digits

aksPrimality :: Integral a => a -> Bool
aksPrimality n = (not $ perfectPower n) && (n <= r || step3) && step5
	where
		logSquared = truncate $ (logBase 2 $ fromIntegral n)^2
		r = until (\r -> (not $ coprime n r) || (multiplicativeOrder n r)>logSquared) (+1) 2
		step3 = all (\a -> a `mod` n /= 0) [2.. min r $ n-1]
		maxA = truncate $ (sqrt $ fromIntegral $ totient r)*(logBase 2 $ fromIntegral n)
		step5 = all (\a -> modularPow a n n == a) [1.. maxA]

fermatTest :: (Integral a, Random a) => Int -> a -> IO Bool
fermatTest k n = fmap (all (\a -> modularPow a (n-1) n == 1) . take k . randomRs (2,n-2)) getStdGen