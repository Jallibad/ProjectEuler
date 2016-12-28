import Data.Function
import Data.List
import Data.Ratio
import qualified Data.Map as M
import MathFunctions
import Data.Ord (comparing)
import Data.Array

{-
--Taken from "http://programmingpraxis.com/2012/07/10/sieving-for-totients/" NOT MY WORK
totients :: Integral a => a -> [a]
totients n = M.elems $ foldl (\m i -> if m M.! i == i then foldr (M.adjust (\x -> x*(i-1) `div` i)) m [i,2*i..n] else m)
	(M.fromList $ zip [0..n] [0..n]) [2..n]
-}

totients :: Integer -> [(Integer, Integer)]
totients upperBound = drop 2 $ assocs $ foldl f (listArray (0, upperBound-1) [0..]) [2..upperBound-1]
	where f arr i
		| arr ! i /= i = arr
		| otherwise = accum (\j _ -> truncate $ ((fromIntegral j) / (fromIntegral i))*(fromIntegral $ i-1)) arr $ zip [i,2*i..upperBound-1] $ repeat i

main = print $ fst $ minimumBy (comparing $ uncurry ((/) `on` fromIntegral)) $ filter (\(x, y) -> (sort $ show x) == (sort $ show y)) $ totients $ 10^7