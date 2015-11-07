isqrt :: Integral (a) => a -> a
isqrt 0 = 0
isqrt 1 = 1
isqrt n = head $ dropWhile (\x -> x*x > n) $ iterate (\x -> (x + n `div` x) `div` 2) (n `div` 2)

isPrime :: Integral (a) => a -> Bool
isPrime 1 = False
isPrime number = all ((/=) 0 . mod number) [2..isqrt number]

main = print $ filter isPrime [1..] !! 10000