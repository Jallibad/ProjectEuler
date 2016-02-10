import Data.Char (digitToInt)
import ListFunctions
import Data.Function

digitSum :: Integral (a) => Show (a) => a -> a
digitSum n = fromIntegral $ sum $ map digitToInt $ show n

isInt x = x == fromInteger (round x)

--isA :: Integral (a) => Show (a) => a -> Bool
isA :: Integer -> Bool
isA n
	| n < 10 = False
	| sumOfDigits == 1 = False
	| otherwise = n == (sumOfDigits^(round $ (logBase `on` fromIntegral) sumOfDigits n))
--	| otherwise = any (==n) $ map (sumOfDigits^) [1..20]
	where sumOfDigits = digitSum n

--powers n previous = x ++ (orderedUnion xs $ powers (n+1))
--	where	(x,xs) = span (<=2^(n+1)) previous

powers = foldl1 orderedUnion $ map (\x -> map (^x) [1..10^7]) [2..50]

main = print $ (filter isA powers) !! (30-1)