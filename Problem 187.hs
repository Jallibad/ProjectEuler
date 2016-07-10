import MathFunctions

upperBound = 10^8

main = print $ length [x*y | x <- takeWhile (<(isqrt $ 10^8)) primes, y <- takeWhile ((<10^8) . (x*)) $ dropWhile (<x) primes]