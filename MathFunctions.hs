{-# LANGUAGE ViewPatterns, TupleSections, BangPatterns #-}

module MathFunctions where

import Control.Arrow ((&&&), second)
import Control.Monad (filterM)
import Data.Bits (Bits, testBit)
import Data.Char (digitToInt)
import Data.Function ((&), on)
import Data.Function.Memoize
import Data.List (genericLength, intersect, nub, tails, unfoldr)
import Data.List.Ordered (member)
import Data.Maybe (maybe)
import Data.Ratio ((%))
import qualified Data.Set as Set
import Data.Tuple (swap)
import ListFunctions (circulateList)
import System.Random (getStdGen, Random, randomRs)

andAll :: [a -> Bool] -> a -> Bool
andAll = (. (&)) . flip all

applyIf :: (a -> a) -> Bool -> a -> a
applyIf f True = f
applyIf _ _ = id

unless :: a -> Bool -> Maybe a
_ `unless` True	= Nothing
v `unless` _	= Just v

isqrt :: Integral a => a -> a
isqrt 0 = 0
isqrt 1 = 1
isqrt n = head $ dropWhile (\x -> x*x > n) $ iterate (\x -> (x + n `div` x) `div` 2) (n `div` 2)

amicable :: Integral a => a -> a
amicable = sum . initFactors

isAmicable :: Integral a => a -> Bool
isAmicable a = a == amicable b && a /= b
	where b = amicable a

isPrime :: Integral a => a -> Bool
isPrime ((`elem` [-1,0,1]) -> True) = False
isPrime (abs -> n) = all ((/=) 0 . mod n) [2..isqrt n]

coprime :: Integral a => a -> a -> Bool
coprime = ((/= 0) .) . mod

isCircularPrime :: (Integral a, Show a) => a -> Bool
isCircularPrime n
	| n < 10 = isPrime n
	| otherwise = null (intersect invalids s) && (all (isPrime . read) $ circulateList s)
	where
		invalids = "024568"
		s = show n

isTruncatablePrime :: (Integral a, Show a) => a -> Bool
isTruncatablePrime = andAll [(>= 10), left, right]
	where
		left = all isPrime . takeWhile (/=0) . iterate (`div` 10)
		right = all (isPrime . read) . init . tails . show

primes :: Integral a => [a]
primes = 2 : 3 : filter isPrime (concatMap (\k -> [6*k-1,6*k+1]) [1..])

isSquare :: Integral a => a -> Bool
isSquare n = ((n `mod` 20) `elem` validPrimes) && ((isqrt n)^2 == n)
	where validPrimes = [0,1,4,5,9,16]

factors :: Integral a => a -> [a]
factors 0 = []
factors n = uncurry (++) $ second (applyIf tail (isSquare n) . reverse) $ unzip
			[(x,a) | (x, (a,0)) <- map (id &&& quotRem n) [1..isqrt n]]

initFactors :: Integral a => a -> Set.Set a
initFactors = Set.deleteMax . Set.fromDistinctAscList . factors

primeFactors :: Integral a => a -> [a]
primeFactors = unfoldr $ \x ->
				(id &&& div x $ head $ dropWhile (coprime x) primes) `unless` (x==1)

distinctPrimeFactors = nub . primeFactors

factorial :: Integral a => a -> a
factorial 0 = 1
factorial n = product [1..n]

factorials :: Integral a => [a]
factorials = 1 : scanl1 (*) [1..]

digitFactorial :: Integral a => a -> a
digitFactorial = sum . map factorial . digits

digits :: Integral a => a -> [a]
digits = digits' 10

digits' :: Integral a => a -> a -> [a]
digits' m = reverse . unfoldr (\n -> (swap $ n `quotRem` m) `unless` (n==0))

combinatoric :: Integral (a) => a -> a -> a
combinatoric n k = truncate $ product $ [(n+1-i) % i | i <- [1.. min k $ n-k]]

fibs :: Integral a => [a]
fibs = map fromIntegral fibs'
	where fibs' = 0 : 1 : zipWith (+) fibs' (tail fibs')

intToBinary :: (Integral a, Bits a) => a -> [Bool]
intToBinary n = map (testBit n) [0..truncate $ logBase 2 $ fromIntegral n]

primitivePythagoreanTriples = [
	(a,b,c) |
		m <- [1..],
		n <- [(ceiling $ (sqrt 2)*(fromIntegral $ abs m))-m..m-1],
		even m || even n,
		coprime m n,
		let a=m^2-n^2,
		let b=2*m*n,
		let c=m^2+n^2
	]

stern_brocot_sequence :: Integral a => (a -> a) -> a -> a
stern_brocot_sequence _ 0 = 0
stern_brocot_sequence _ 1 = 1
stern_brocot_sequence f n = f halfN + if odd n then f (halfN+1) else 0
	where halfN = n `div` 2

stern_brocot_sequenceM :: (Integral a, Memoizable a) => a -> a
stern_brocot_sequenceM = memoFix stern_brocot_sequence

(↑↑) :: Integral a => a -> a -> a
(↑↑) = (foldr1 (^) .) . flip (replicate . fromIntegral)

modularPow :: (Integral a, Integral b) => a -> b -> a -> a
modularPow _ 0 _ = 1
modularPow b e m = if odd e then mult result b else result
	where
		result = modularPow (mult b b) (e `div` 2) m
		mult x y = (x*y) `mod` m

perfectPower :: Integral a => a -> Bool
perfectPower n = any (\m -> member n $ map (m^) primes) [2.. isqrt n]

multiplicativeOrder :: Integral a => a -> a -> a
multiplicativeOrder a n = head $ dropWhile (\x -> (modularPow a x n)/=1) [1..n]

totient :: Integral a => a -> a
totient n = genericLength $ filter (coprime n) [1..n]

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