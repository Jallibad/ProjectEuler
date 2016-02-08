import MathFunctions
import qualified Data.Set as Set
import Data.List

{-
squares = map (^2) [1..]

f x
	| x `mod` 4 /= 0 = 0
	| otherwise = length $ filter (\y -> let n=y-x in isSquare n && ((even y) == (even n))) $ takeWhile (<=((x+4) `div` 4)^2) $ dropWhile (<=x) squares
-}

factorsLessThanSqrt n = length $ filter ((==0) . mod n) $ takeWhile (\x -> (fromIntegral x) < (sqrt $ fromIntegral n)) [1..]

main = print $ (scanl1 (+) $ map factorsLessThanSqrt [1..]) !! (10^6 `div` 4 - 1)