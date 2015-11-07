import Data.Function
import Fraction

fractions = map (\x -> Fraction 1 x^2) [2..9]

addFractions :: Fraction -> [Fraction] -> Int
addFractions (Fraction 1 2) _ = 1
addFractions _ [] = 0
addFractions f fs
	| f > (Fraction 1 2) = 0
	| otherwise = sum $ map (\x -> addFractions (x+f) $ filter (/=x) fs) fs