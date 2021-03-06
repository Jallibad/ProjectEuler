import Data.List
import Data.Ord

rectanglesInGrid :: Integral (a) => a -> a -> a
rectanglesInGrid n m = n*(n+1)*m*(m+1) `div` 4

main = print $ fst $ minimumBy (comparing snd) [(x*y, abs $ 2*10^6 - (rectanglesInGrid x y)) | x <- [1..2000], y <- [1..2000]]