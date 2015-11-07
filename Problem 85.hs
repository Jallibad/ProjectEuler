import Data.List
import Data.Function

rectanglesInGrid :: Integral (a) => a -> a -> a
rectanglesInGrid n m = (n*(n+1))*(m*(m+1)) `div` 4

main = print $ fst $ minimumBy (compare `on` (\(_, x) -> abs (2*10^6 - x))) [(x*y, rectanglesInGrid x y) | x <- [1..2000], y <- [1..2000]]