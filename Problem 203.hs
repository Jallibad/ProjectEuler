import Data.List
import MathFunctions

primeSquares = map (^2) primes

squareFree :: Integer -> Bool
squareFree n = all (\x -> n `mod` x /= 0) $ takeWhile (<=n) primeSquares

main = print $ sum $ filter squareFree $ nub [combinatoric n k | n <- [0..50], k <- [0..n]]