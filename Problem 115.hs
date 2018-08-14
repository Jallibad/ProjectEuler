import Data.Function.Memoize (memoFix2)

fill_count :: Integer -> Integer
fill_count = memoFix2 tiles 50

tiles f m n
	| n < m = 1
	| otherwise = sum $ map (\i -> f m (if i < n then n-i-1 else n-i)) (0:[m..n])

main = print $ until ((>=10^6) . fill_count) (+1) 50