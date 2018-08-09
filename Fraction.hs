module Fraction where

import Data.Function
import qualified Data.Ratio as R

data Fraction = Fraction {numerator :: Integer, denominator :: Integer}

toRatio (Fraction a b) = a R.% b

--value :: Fractional (a) => Int -> Int -> a
value = fromRational . toRatio

instance Show Fraction where
	show (Fraction n d) = (show n)++"/"++(show d)

simplify :: Fraction -> Fraction
simplify (Fraction n d) = Fraction (n `div` s) (d `div` s)
	where s = gcd n d

instance Eq Fraction where
	f1 == f2 = (a==c) && (b==d)
		where
			(Fraction a b) = simplify f1
			(Fraction c d) = simplify f2

instance Ord Fraction where
	compare (Fraction a b) (Fraction c d) = compare (a |/ b) (c |/ d)
		where (|/) = (/) `on` fromIntegral

instance Num Fraction where
	(Fraction a b) + (Fraction c d) = simplify $ Fraction (a*d+b*c) (b*d)
	(Fraction a b) * (Fraction c d) = simplify $ Fraction (a*c) (b*d)
	abs (Fraction a b) = Fraction (abs a) (abs b)
	signum (Fraction a b) = Fraction (signum a * signum b) 1
	fromInteger n = Fraction n 1
	negate (Fraction a b) = Fraction (-a) b

instance Fractional Fraction where
	recip (Fraction a b) = Fraction b a
	fromRational r = Fraction (fromIntegral $ R.numerator r) (fromIntegral $ R.denominator r)

nextFarey :: Integer -> Fraction -> Fraction -> Fraction
nextFarey n (Fraction a b) (Fraction c d) = Fraction p q
	where
		k = (n+b) `div` d
		p = k*c-a
		q = k*d-b

fareyLength n = (n+3)*n `div` 2 - (sum $ map (\d -> fareyLength $ floor (n `div` d)) [2..n])

mediant (Fraction a b) (Fraction c d) = Fraction (a+c) (b+d)