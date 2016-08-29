import MathFunctions

diagonals = [1] : map part [3,5..]
	where part n = map (\x -> n^2-x*(n-1)) [3,2,1,0]

main = print $ fst $ head $ dropWhile (\(_, (x, y)) -> x*10>=y) $ zip [3,5..] $ drop 2 $ scanl (\(primes, total) current -> (primes+(length $ filter isPrime current), total+(length current))) (0, 0) diagonals