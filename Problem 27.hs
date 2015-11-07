import Data.List (maximumBy)
import Data.Ord (comparing)

isqrt :: Integral (a) => a -> a
isqrt 0 = 0
isqrt 1 = 1
isqrt n = head $ dropWhile (\x -> x*x > n) $ iterate (\x -> (x + n `div` x) `div` 2) (n `div` 2)

isPrime :: Integral (a) => a -> Bool
isPrime 1 = False
isPrime realNumber = all ((/=) 0 . mod number) [2..isqrt number]
	where number = abs realNumber

consecutives :: Integral (a) => a -> a -> Int
consecutives a b = length $ takeWhile isPrime $ map (\n -> n^2 + a*n + b) [0..]

main = print $ fst $ maximumBy (comparing snd) [(a*b, consecutives a b) | a <- [(-1000)..1000], b <- [(-1000)..1000]]