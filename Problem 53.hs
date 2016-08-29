import MathFunctions (combinatoric)

main = print $ length [x | n <- [23..100], r <- [2..n-1], let x=combinatoric n r, x>10^6]