import MathFunctions (combinatoric)

main = print $ length $ filter (>10^6) [combinatoric n r | n <- [1..100], r <- [1..n-1]]