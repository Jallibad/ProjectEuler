import Data.Function
import Fraction

fractionsBetween :: Fraction -> Fraction -> Integer -> Integer
fractionsBetween f1 f2 dBound
	| denominator m > dBound = 0
	| otherwise = (fractionsBetween f1 m dBound)+(fractionsBetween m f2 dBound)+1
	where m = mediant f1 f2

main = print $ fractionsBetween (Fraction 1 3) (Fraction 1 2) 12000