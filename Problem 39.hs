import qualified Data.Map.Strict as Map
import Data.List (maximumBy)
import Data.Ord (comparing)

isqrt 0 = 0
isqrt 1 = 1
isqrt n = head $ dropWhile (\x -> x*x > n) $ iterate (\x -> (x + n `div` x) `div` 2) (n `div` 2)

isSquare n = n == (isqrt n)^2

addNumber :: Map.Map Int Int -> Int -> Map.Map Int Int
addNumber map n =
	if n `Map.member` map
	then Map.update (Just . (+) 1) n map
	else Map.insert n 1 map

triangleNumbers :: [Int]
triangleNumbers = [p | a <- [1..1000], b <- [1..a-1], let c=a^2+b^2, isSquare c, let p=b+a+ isqrt c, p<=1000]

main = print $ maximumBy (comparing snd) $ Map.toList $ foldr (\x map -> addNumber map x) Map.empty triangleNumbers