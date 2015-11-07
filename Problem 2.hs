main = print $ sum $ filter even $ takeWhile (<4*10^6) $ fibs
	where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)