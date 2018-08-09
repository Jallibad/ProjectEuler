import Data.Char (digitToInt)

champernowneConstant = concatMap show [1..]

main = print $ product $ map (\x -> digitToInt $ champernowneConstant !! (10^x-1)) [1..6]