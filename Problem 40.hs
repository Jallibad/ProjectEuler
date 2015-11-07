import Data.Char (digitToInt)

champernowneConstant = concatMap show [1..]

main = print $ product $ map (digitToInt . (!!) champernowneConstant) [10^x-1 | x <- [1..6]]