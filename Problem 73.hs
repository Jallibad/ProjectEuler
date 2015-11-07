import Data.Function
import Fraction

main = print $ length $ takeWhile (< (Fraction 1 2)) $ dropWhile (<= (Fraction 1 3)) $ map snd $ iterate (\(f1, f2) -> (f2, nextFarey n f1 f2)) ((Fraction 0 1), (Fraction 1 n))
	where n = 12000