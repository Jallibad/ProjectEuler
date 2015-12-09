import MathFunctions
import Data.Function (on)

diagonals = map part [1,3..]
	where	part 1 = [1]
		part n = map (\x -> n^2-x*(n-1)) [3,2,1,0]

main = print $ fst $ head $ dropWhile (\(_, (x, y)) -> x*10>=y) $ tail $ zip [1,3..] $ tail $ scanl (\(primes, total) current -> (primes+(length $ filter isPrime current), total+(length current))) (0, 0) diagonals