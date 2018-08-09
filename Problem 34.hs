import MathFunctions (digitFactorial)

maxNumber :: Int
maxNumber = 10^(until (\x -> (362880*x) < (10^x-1)) (+1) 1)-1

main = print $ sum $ filter (\x -> x == digitFactorial x) [3..maxNumber]