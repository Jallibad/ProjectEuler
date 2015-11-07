diagonals :: Integral (a) => a -> [a]
diagonals 1 = [1]
diagonals n = map (\x -> n^2-x*(n-1)) [0..3]

main = print $ sum $ concatMap diagonals [1,3..1001]