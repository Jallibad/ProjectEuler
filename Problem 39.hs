import Data.List (maximumBy)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)

addNumber :: Int -> Map.Map Int Int -> Map.Map Int Int
addNumber n map = Map.insertWith (+) n 1 map

triangleNumbers :: [Int]
triangleNumbers = concatMap (\x -> takeWhile (<1000) $ map (*x) [1..]) $ takeWhile (<1000) [2*m*(m+n) | m <- [1..], n <- [(ceiling $ (sqrt 2)*(fromIntegral $ abs m))-m..m-1]]

main = print $ fst $ maximumBy (comparing snd) $ Map.toList $ foldr addNumber Map.empty triangleNumbers