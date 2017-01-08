import FigurateNumbers (pentagonals)

isInt x = x == fromInteger (round x)

isPentagonal x = isInt $ ((sqrt $ 24*(fromIntegral x)+1)+1) / 6

main = print $ head [k-j | k <- pentagonals, j <- takeWhile (<k) pentagonals, isPentagonal $ k+j, isPentagonal $ k-j]