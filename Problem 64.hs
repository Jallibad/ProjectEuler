import Data.List (zipWith, maximumBy, (\\))
import Data.Function (on)
import Data.Ord (comparing)

--Used "http://mathworld.wolfram.com/PellEquation.html" for research on the Pell equation

solveEquation d = length $ takeWhile (/=(head a * 2)) a
	where
		a = (truncate $ sqrt $ fromIntegral d) : zipWith (div) (map (+(head a)) $ tail _p) (tail _q)
		p = (head a) : ((a !! 0)*(a !! 1))+1 : zipWith (+) (zipWith (*) (drop 2 a) (tail p)) p
		q = 1 : (a !! 1) : zipWith (+) q (zipWith (*) (drop 2 a) (tail q))
		_p = 0 : (head a) : zipWith (-) (zipWith (*) (tail a) (tail _q)) (tail _p)
		_q = 1 : (d - (head a)^2) : zipWith (div) (map (\x -> d-x^2) $ drop 2 _p) (tail _q)

nonSquares = [2..limit] \\ (takeWhile (<=limit) $ map (^2) [2..])
	where limit = 10000

main = print $ length $ filter odd $ map solveEquation nonSquares