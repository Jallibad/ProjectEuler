main = do
	file <- readFile "Problem 13 Large Numbers.txt"
	print $ take 10 $ show $ sum $ map read $ lines file