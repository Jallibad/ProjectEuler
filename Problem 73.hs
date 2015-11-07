import Data.Function

data Fraction = Fraction {n :: Int, d :: Int}

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

nextFarey :: Int -> Fraction -> Fraction -> Fraction
nextFarey n (Fraction a b) (Fraction c d) = Fraction p q
	where
		k = (n+b) `div` d
		p = k*c-a
		q = k*d-b

main = print $ length $ takeWhile (< (Fraction 1 2)) $ dropWhile (<= (Fraction 1 3)) $ map snd $ iterate (\(f1, f2) -> (f2, nextFarey n f1 f2)) ((Fraction 0 1), (Fraction 1 n))
	where n = 12000