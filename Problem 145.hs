import Data.Char (digitToInt)

isReversible :: Integral (a) => Show (a) => Read (a) => a -> Bool
isReversible n
	| last s == '0' = False
	| otherwise = all (odd . digitToInt) $ show $ n + (read $ reverse s)
	where s = show n

main = print $ length $ filter isReversible [1..10^9-1]