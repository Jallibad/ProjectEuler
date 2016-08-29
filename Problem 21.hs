import MathFunctions (isAmicable)

main = print $ sum $ filter isAmicable [1..10^4-1]