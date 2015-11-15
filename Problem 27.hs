import Data.List (maximumBy)
import Data.Ord (comparing)
import MathFunctions

consecutives :: Integral (a) => a -> a -> Int
consecutives a b = length $ takeWhile isPrime $ map (\n -> n^2 + a*n + b) [0..]

main = print $ fst $ maximumBy (comparing snd) [(a*b, consecutives a b) | a <- [(-1000)..1000], b <- [(-1000)..1000]]