import Data.Function.Memoize (memoFix)

tiles :: (Num a, Ord a) => (a -> a) -> a -> a
tiles _ 0 = 1
tiles f n
	| n < 0 = 0
	| otherwise = sum $ map f [n-1, n-2, n-3, n-4]

main = print $ memoFix tiles (50 :: Integer)