main = print result
	where result = sum $ map (\a -> maximum $ take (fromIntegral a) $ map (\n -> ((a-1)^n+(a+1)^n) `mod` (a^2)) [1,3..]) [3..10^3] :: Integer