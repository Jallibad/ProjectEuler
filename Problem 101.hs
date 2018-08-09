import Data.List
import qualified Data.Map.Strict as Map
import Data.Ratio
import Data.Function

newtype Polynomial = Polynomial (Map.Map Integer Rational) deriving Eq

getMap :: Polynomial -> Map.Map Integer Rational
getMap (Polynomial p) = p

instance Num Polynomial where
	(+) = ((Polynomial . Map.filter (/=0)) .) . (Map.unionWith (+) `on` getMap)
	(Polynomial p1) * (Polynomial p2) = Polynomial $ Map.filter (/=0) $ Map.unionsWith (+) $ Map.foldlWithKey (\m c v -> (Map.mapKeysMonotonic (c+) $ fmap (v*) p2):m) [] p1
	abs = Polynomial . fmap abs . getMap
	signum = Polynomial . Map.singleton 0 . signum . snd . Map.findMax . getMap
	fromInteger = Polynomial . Map.singleton 0 . fromInteger
	negate = Polynomial . Map.updateMax (Just . negate) . getMap

instance Show Polynomial where
	show = tail . Map.foldlWithKey (\s k a -> ' ':(show a)++"x^"++(show k)++s) "" . getMap

constructPolynomial :: [Rational] -> Polynomial
constructPolynomial = Polynomial . Map.fromDistinctAscList . zip [0..]

interpolate :: [Integer] -> Polynomial
interpolate y = sum $ map coefficient kRange
	where
		kRange = [0..genericLength y - 1]
		coefficient j = product $ (fromInteger $ y `genericIndex` j) : (map (coefficientTerm j) $ delete j kRange)
		coefficientTerm j m = constructPolynomial [(-(m+1)) % (j-m), 1 % (j-m)]

eval :: Polynomial -> Rational -> Rational
eval p x = Map.foldrWithKey (\k a -> (x^k*a+)) 0 $ getMap p

bop :: Rational -> [Integer] -> Integer
bop k xs = numerator $ eval (interpolate xs) k

main = print $ sum $ map (uncurry bop) $ zip [2..] $ tail $ inits $ map (\n -> 1-n+n^2-n^3+n^4-n^5+n^6-n^7+n^8-n^9+n^10) [1..10]