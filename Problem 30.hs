import Data.Char (digitToInt)

digitPower :: Integral (a) => Show (a) => a -> a -> Int
digitPower n p = sum $ map (\x -> (digitToInt x)^p) $ show n

main = print $ sum $ filter (\x -> digitPower x power == x) [2..upperBound]
	where
		power = 5
		upperBound = 6*9^power