import MathFunctions

--Formula found at "http://oeis.org/A046090"

a :: [Integer]
a = 1 : 4 : (zipWith (\a2 a1 -> 6*a1 - a2 - 2) a $ tail a)

main = print $ (\n -> ((isqrt $ 1+4*(n*(n-1) `div` 2))+1) `div` 2) $ head $ dropWhile (<10^12) a