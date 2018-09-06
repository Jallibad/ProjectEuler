import Data.List (maximumBy)
import Data.Ord (comparing)
import PrimeFunctions (isPrime, primes)

consecutives :: Integral (a) => a -> a -> Int
consecutives a b = length $ takeWhile isPrime $ map (\n -> n^2 + a*n + b) [1..]

main = print $ fst $ maximumBy (comparing snd) [(a*b, consecutives a b) | b <- takeWhile (<1000) primes, a <- [(-b),(-b)+2..1000]]