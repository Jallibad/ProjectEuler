import Data.List
import Data.Ratio
import Polynomial

interpolate :: [Integer] -> Polynomial Integer Rational
interpolate y = sum $ map coefficient kRange
	where
		kRange = [0..genericLength y - 1]
		coefficient j = product $ (fromInteger $ y `genericIndex` j) : (map (coefficientTerm j) $ delete j kRange)
		coefficientTerm j m = constructPolynomial [(-(m+1)) % (j-m), 1 % (j-m)]

bop :: Rational -> [Integer] -> Integer
bop k xs = numerator $ eval (interpolate xs) k

main = print $ sum $ map (uncurry bop) $ zip [2..] $ tail $ inits $ map (\n -> 1-n+n^2-n^3+n^4-n^5+n^6-n^7+n^8-n^9+n^10) [1..10]