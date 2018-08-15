import Data.List
import MathFunctions
import qualified Data.Set as Set

primeSquares = map (^2) primes

squareFree :: Integer -> Bool
squareFree n = all (\x -> n `mod` x /= 0) $ takeWhile (<=n) primeSquares

main = print $ sum $ Set.filter squareFree $ Set.fromList [combinatoric n k | n <- [0..50], k <- [0..n]]