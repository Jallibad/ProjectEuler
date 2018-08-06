import Control.Arrow
import Data.Function (on)
import Data.List (zipWith, maximumBy, (\\))
import Data.Ord (comparing)

import MathFunctions (isqrt)

--Used "http://mathworld.wolfram.com/PellEquation.html" for research on the Pell equation

solveEquation :: Integral (a) => a -> a
solveEquation d = p !! (if odd r then r else 2*r+1)
	where
		a@(a0:a1:_) = (isqrt d) : zipWith div (map (+a0) _p) _q
		p = a0 : a0*a1+1 : zipWith (+) (zipWith (*) (drop 2 a) $ tail p) p
		q = a1 : zipWith (+) (1 : q) (zipWith (*) q $ drop 2 a)
		_p = a0 : zipWith (-) (zipWith (*) (tail a) _q) _p
		_q = (d - a0^2) : zipWith div (map (\x -> d-x^2) $ tail _p) _q
		r = (length $ takeWhile (/=(a0*2)) a)-1

nonSquares limit = [1..limit] \\ (takeWhile (<=limit) $ map (^2) [1..])

main = print $ maximumBy (comparing solveEquation) $ nonSquares 1000