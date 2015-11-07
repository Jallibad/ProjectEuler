factorials = 1 : (scanl1 (*) [1..])

factorial :: Int -> Integer
factorial n = factorials !! n

combinatoric :: Integral (a) => Int -> Int -> Integer
combinatoric n r = (factorial n) `div` ((factorial (n-r))*(factorial r))

main = print $ length $ filter (>10^6) [combinatoric n r | n <- [1..100], r <- [1..n-1]]