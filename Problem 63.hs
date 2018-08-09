import ListFunctions (takeRange)

main = print $ sum $ takeWhile (/=0) $ map (\n -> length $ takeRange (10^(n-1)) (10^n) $ map (^n) [1..]) [1..]