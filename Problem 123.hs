import MathFunctions (primes)

main = print $ fst $ head $ dropWhile (\(n, p) -> ((p-1)^n+(p+1)^n) `mod` (p^2) <=10^10) $ zip [1..] primes