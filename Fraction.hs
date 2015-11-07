module Fraction where

import Data.Function

data Fraction = Fraction {numerator :: Integer, denominator :: Integer}

--value :: Fractional (a) => Int -> Int -> a
value (Fraction n d) = ((/) `on` fromIntegral) n d

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
	signum (_) = Fraction 1 1 --TODO: Fix
	fromInteger n = Fraction n 1
	negate (Fraction a b) = Fraction (-a) b