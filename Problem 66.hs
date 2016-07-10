import Data.List (zipWith, maximumBy, (\\))
import Data.Function (on)
import Data.Ord (comparing)

--Used "http://mathworld.wolfram.com/PellEquation.html" for research on the Pell equation

solveEquation :: Integral (a) => a -> a
solveEquation d
	| odd r = p !! r
	| otherwise = p !! (2*r+1)
	where
		a = (truncate $ sqrt $ fromIntegral d) : zipWith div (map (+(head a)) $ tail _p) (tail _q)
		p = (head a) : ((a !! 0)*(a !! 1))+1 : zipWith (+) (zipWith (*) (drop 2 a) (tail p)) p
		q = 1 : (a !! 1) : zipWith (+) q (zipWith (*) (drop 2 a) (tail q))
		_p = 0 : (head a) : zipWith (-) (zipWith (*) (tail a) (tail _q)) (tail _p)
		_q = 1 : (d - (head a)^2) : zipWith div (map (\x -> d-x^2) $ drop 2 _p) (tail _q)
		r = (length $ takeWhile (/=(head a * 2)) a)-1

nonSquares = [1..limit] \\ (takeWhile (<=limit) $ map (^2) [1..])
	where limit = 1000

main = print $ fst $ maximumBy (comparing snd) $ zip nonSquares $ map solveEquation nonSquares