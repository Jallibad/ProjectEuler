import MathFunctions (primes)

main = print $ fst $ head $ dropWhile ((<=10^10) . snd) $ map (\(n, p) -> (n, ((p-1)^n+(p+1)^n) `mod` (p^2))) $ zip [1..] primes