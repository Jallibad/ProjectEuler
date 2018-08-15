{-# LANGUAGE ViewPatterns #-}

import Data.Char (intToDigit)
import Data.Function.Memoize (memoFix2, memoize)
import Data.List (permutations)
import qualified MathFunctions as Math (isPrime)

intListToInt :: [Int] -> Int
intListToInt = read . map intToDigit

isPrime = memoize Math.isPrime

primeSet' :: (Int -> [Int] -> Int) -> Int -> [Int] -> Int
primeSet' _ _ [] = 1
primeSet' f g xs = sum [f a b | x <- [1..length xs], let (intListToInt -> a,b) = splitAt x xs, a > g && isPrime a]

primeSet :: [Int] -> Int
primeSet = memoFix2 primeSet' 0

main = print $ sum $ map primeSet $ permutations [1..9]