import MathFunctions

amicable :: Integral (a) => a -> a
amicable 0 = 0
amicable n = sum $ init $ factors n

isAmicable :: Integral (a) => a -> Bool
isAmicable a = a == amicable b && a /= b
	where b = amicable a

main = print $ sum $ filter isAmicable [1..10^4-1]