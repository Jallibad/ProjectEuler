import MathFunctions (primes)

upperBound = 10^9

f x = takeWhile (<upperBound) $ map (x^) [0..]

main = print $ length $ foldl1 (\f1 f2 -> concatMap (\x -> takeWhile (<=upperBound) $ map (x*) f2) f1) $ map f $ takeWhile (<=100) primes