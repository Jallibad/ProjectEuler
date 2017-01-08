import Data.List

main = print $ sum $ map (\a -> maximum $ genericTake a $ map (\n -> ((a-1)^n+(a+1)^n) `mod` (a^2)) [1,3..]) [3..10^3]