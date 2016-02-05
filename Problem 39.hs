import qualified Data.Map.Strict as Map
import Data.List (maximumBy)
import Data.Ord (comparing)
import MathFunctions (isqrt, isSquare)

addNumber :: Int -> Map.Map Int Int -> Map.Map Int Int
addNumber n map =
	if n `Map.member` map
	then Map.update (Just . (+) 1) n map
	else Map.insert n 1 map

triangleNumbers :: [Int]
triangleNumbers = [p | a <- [1..1000], b <- [1..a-1], let c=a^2+b^2, isSquare c, let p=a+b+ isqrt c, p<=1000]

main = print $ fst $ maximumBy (comparing snd) $ Map.toList $ foldr addNumber Map.empty triangleNumbers