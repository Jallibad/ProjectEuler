import Data.List
import qualified Data.Map.Strict as Map
import Fraction

multiplyPolynomial p1 p2 = Map.filter (/=0) $ Map.unionsWith (+) $ Map.foldlWithKey (\m c v -> (Map.mapKeysMonotonic (c+) $ Map.map (v*) p2):m) [] p1

interpolate :: [Integer] -> Map.Map Integer Fraction
interpolate y = Map.filter (/=0) $ Map.unionsWith (+) $ map (\j -> foldl multiplyPolynomial (Map.singleton 0 (fromInteger $ y `genericIndex` j)) $ map (\m -> Map.fromList [(1, Fraction 1 (j-m)), (0, simplify $ Fraction (-(m+1)) (j-m))]) $ delete j [0..k]) [0..k]
	where k = genericLength y - 1

eval p x = Map.foldrWithKey (\k a -> (x^k*a+)) 0 p

bop k xs = numerator $ eval (interpolate xs) (Fraction k 1)

main = print $ sum $ map (uncurry bop) $ zip [2..] $ tail $ inits $ map (\n -> 1-n+n^2-n^3+n^4-n^5+n^6-n^7+n^8-n^9+n^10) [1..10]