{-# LANGUAGE ViewPatterns #-}

import Control.Arrow (first)
import Data.Char (intToDigit)
import Data.Function.Memoize-- (memoFix2, memoize)
import Data.List
import Data.Maybe
import qualified PrimeFunctions as Prime (isPrime)
import Debug.Trace

type DigitsSet = [Int]

intListToInt :: DigitsSet -> Int
intListToInt = read . concatMap show

isPrime :: Int -> Bool
isPrime = memoize Prime.isPrime

primeSet' :: (Int -> DigitsSet -> Int) -> Int -> DigitsSet -> Int
primeSet' _ _ [] = 1
primeSet' (uncurry -> f) lastNum digits = sum $ map f $ filter (discard . fst) $ zip numbersTaken setsRemaining
	where
		numbersTaken = traceShowId $ map intListToInt $ tail $ inits digits
		setsRemaining = tail $ init $ tails digits
		discard a = a > lastNum && isPrime a

primeSet'' _ [] = 1
primeSet'' f xs = sum [primeSet'' a b | x <- [1..length xs], let (intListToInt -> a,b) = splitAt x xs, a > f && isPrime a]

primeSet :: DigitsSet -> Int
primeSet = memoFix2 primeSet' 0

main = print $ sum $ map primeSet $ permutations [1..9]