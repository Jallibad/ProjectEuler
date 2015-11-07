pellNumbers :: [Integer]
pellNumbers = 0 : 1 : zipWith (\x y -> x+2*y) pellNumbers (tail pellNumbers)

pellNumerators :: [Integer]
pellNumerators = 0 : zipWith (+) pellNumbers (tail pellNumbers)

main = print $ length $ filter (\(n, d) -> (length $ show n) > (length $ show d)) $ take 1000 $ drop 2 $ zip pellNumerators pellNumbers