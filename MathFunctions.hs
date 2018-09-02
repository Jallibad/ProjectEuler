{-# LANGUAGE ViewPatterns, TupleSections #-}

module MathFunctions where

import Control.Arrow
import Control.Monad (filterM)
import Data.Array
import Data.Bits
import Data.Char (digitToInt)
import Data.Function.Memoize
import Data.List
import Data.List.Ordered (member)
import Data.Ratio
import Data.Tuple (swap)
import ListFunctions
import Polynomial
import Sieve
import System.Random

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
isCircularPrime (show -> string) = (length string == 1 || invalids \\ string == invalids) && (all (isPrime . read) $ circulateList string)
	where invalids = "024568"

isTruncatablePrime :: (Integral a, Show a) => a -> Bool
isTruncatablePrime n = (n >= 10) && (all (isPrime . read) $ (tail $ inits s)++(init $ tails s))
	where s = show n

primes :: Integral a => [a]
primes = 2 : 3 : filter isPrime (concatMap (\k -> [6*k-1,6*k+1]) [1..])

isSquare n = (isqrt n)^2 == n

factors :: Integral a => a -> [a]
factors number = lowerFactors ++ if isSquare number then (tail upperFactors) else upperFactors
	where
		lowerFactors = filter ((==0) . rem number) [1..isqrt number]
		upperFactors = reverse (map (div number) lowerFactors)

primeFactors :: Integral a => a -> [a]
primeFactors n = reverse $ fst $ until ((==1) . snd) (\(p, x) -> let f = head $ dropWhile ((/=0) . (x `mod`)) primes in (f:p, x `div` f)) ([], n)

distinctPrimeFactors = nub . primeFactors

factorization :: Integral a => a -> [a]
factorization =	unfoldr foldFunction
	where
		foldFunction 1 = Nothing
		foldFunction x = Just $ (id &&& div x) $ head $ dropWhile (\n -> x `mod` n /= 0) primes

factorial :: Integral a => a -> a
factorial 0 = 1
factorial n = product [1..n]

factorials :: Integral a => [a]
factorials = 1 : scanl1 (*) [1..]

digitFactorial :: Integral a => a -> a
digitFactorial = sum . map factorial . unfoldr (\n -> if n==0 then Nothing else Just $ swap $ n `quotRem` 10)

combinatoric :: Integral (a) => a -> a -> a
combinatoric n k = truncate $ product $ [(n+1-i) % i | i <- [1.. min k $ n-k]]

fibs :: Integral a => [a]
fibs = map fromIntegral fibs'
	where fibs' = 0 : 1 : zipWith (+) fibs' (tail fibs')

intToBinary :: (Integral a, Bits a) => a -> [Bool]
intToBinary n = map (testBit n) [0..truncate $ logBase 2 $ fromIntegral n]

primitivePythagoreanTriples = [(a,b,c) | m <- [1..], n <- [(ceiling $ (sqrt 2)*(fromIntegral $ abs m))-m..m-1], even m || even n, gcd m n == 1, let a=m^2-n^2, let b=2*m*n, let c=m^2+n^2]

stern_brocot_sequence :: Integral a => (a -> a) -> a -> a
stern_brocot_sequence _ 0 = 0
stern_brocot_sequence _ 1 = 1
stern_brocot_sequence f n = f halfN + if odd n then f (halfN+1) else 0
	where halfN = n `div` 2

stern_brocot_sequenceM :: (Integral a, Memoizable a) => a -> a
stern_brocot_sequenceM = memoFix stern_brocot_sequence

tetrate :: Integral a => a -> a -> a
tetrate a 1 = a
tetrate a k = a^(tetrate a $ k-1)

(↑↑) :: Integral a => a -> a -> a
(↑↑) = tetrate

mult :: Integral a => a -> a -> a -> a
mult x y b = (x*y) `mod` b

modularPow :: (Integral a, Integral b) => a -> b -> a -> a
modularPow _ 0 _ = 1
modularPow b e m = if odd e then mult result b m else result
	where result = modularPow (mult b b m) (e `div` 2) m

perfectPower :: Integral a => a -> Bool
perfectPower n = any (\m -> member n $ map (m^) primes) [2.. isqrt n]

multiplicativeOrder :: Integral a => a -> a -> a
multiplicativeOrder a n = head $ dropWhile (\x -> (modularPow a x n)/=1) [1..n]

totient :: Integral a => a -> a
totient n = genericLength $ filter (\r -> gcd n r == 1) [1..n]

aksPrimality :: Integral a => a -> Bool
aksPrimality n = (not $ perfectPower n) && (n <= r || step3) && step5
	where
		logSquared = truncate $ (logBase 2 $ fromIntegral n)^2
		r = until (\r -> (gcd n r /= 1) || (multiplicativeOrder n r)>logSquared) (+1) 2
		step3 = all (\a -> a `mod` n /= 0) [2.. min r $ n-1]
		maxA = truncate $ (sqrt $ fromIntegral $ totient r)*(logBase 2 $ fromIntegral n)
		step5 = all (\a -> modularPow a n n == a) [1.. maxA]

fermatTest :: (Integral a, Random a) => Int -> a -> IO Bool
fermatTest k n = fmap (all (\a -> modularPow a (n-1) n == 1) . take k . randomRs (2,n-2)) getStdGen

jacobiSymbol a' n
	| a == 1 = 1
	| even a = (jacobiSymbol 2 n)*(jacobiSymbol (a `div` 2) n)
	| gcd a n /= 1 = 0
--	| otherwise = jacobiSymbol
	where a = a' `mod` n

{-
millerRabinTest :: (Integral a, Random a) => a -> IO Bool
millerRabinTest n = do
	a <- randomRIO (2,n-2)
	let r = truncate $ logBase 2 $ fromIntegral n
-}