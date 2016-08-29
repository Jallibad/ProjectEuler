factorsLessThanSqrt n = length $ filter ((==0) . mod n) $ takeWhile (\x -> (fromIntegral x) < (sqrt $ fromIntegral n)) [1..]

main = print $ sum $ map factorsLessThanSqrt [1..10^6 `div` 4 - 1]