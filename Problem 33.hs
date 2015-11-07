import Data.Function

data Fraction = Fraction {numerator :: Int, denominator :: Int}

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

(|/) :: (Integral a, Fractional c) => a -> a -> c
(|/) = (/) `on` fromIntegral

instance Ord Fraction where
	compare (Fraction a b) (Fraction c d) = compare (a |/ b) (c |/ d)

instance Num Fraction where
	(Fraction a b) + (Fraction c d) = simplify (Fraction (a*d+c*b) (b*d))
	(Fraction a b) - (Fraction c d) = simplify (Fraction (a*d-c*b) (b*d))
	(Fraction a b) * (Fraction c d) = simplify (Fraction (a*c) (b*d))
	abs (Fraction n d) = Fraction (abs n) (abs d)
	signum (Fraction n d) = Fraction ((signum n)*(signum d)) 1
	fromInteger n = Fraction (fromInteger n) 1

main = print $ denominator $ product [Fraction n d | i <- [1..9], d <- [1..9], n <- [1..d-1], i/=n, i/=d, (d*(10*n+i))==(n*(10*i+d))]