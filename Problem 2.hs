import MathFunctions

main = print $ sum $ filter even $ takeWhile (<4*10^6) $ fibs