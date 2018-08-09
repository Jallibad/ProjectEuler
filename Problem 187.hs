import MathFunctions

upperBound = 10^8

main = print $ length [x*y | x <- takeWhile (<(isqrt upperBound)) primes, y <- takeWhile ((<upperBound) . (x*)) $ dropWhile (<x) primes]