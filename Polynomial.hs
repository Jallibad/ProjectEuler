module Polynomial where

import Control.Arrow (first)
import Data.Function (on)
import qualified Data.Map.Strict as Map
import Debug.Trace

data Polynomial a b = Polynomial (Map.Map a b) deriving Eq

getMap :: Polynomial a b -> Map.Map a b
getMap (Polynomial p) = p

instance (Integral a, Num b, Eq b) => Num (Polynomial a b) where
	(+) = (normalize .) . (Map.unionWith (+) `on` getMap)
	(Polynomial p1) * (Polynomial p2) = Polynomial $ Map.filter (/=0) $ Map.unionsWith (+) $ Map.foldlWithKey (\m c v -> (Map.mapKeysMonotonic (c+) $ fmap (v*) p2):m) [] p1
	abs = Polynomial . fmap abs . getMap
	signum = Polynomial . Map.singleton 0 . signum . snd . Map.findMax . getMap
	fromInteger = Polynomial . Map.singleton 0 . fromInteger
	negate = Polynomial . Map.updateMax (Just . negate) . getMap

normalize :: (Num a, Num b, Eq b) => Map.Map a b -> Polynomial a b
normalize m = Polynomial $ if (Map.null noZeros) then Map.singleton 0 0 else noZeros
	where noZeros = Map.filter (/=0) m

instance (Show a, Show b) => Show (Polynomial a b) where
	show = tail . Map.foldlWithKey (\s k a -> ' ':(show a)++"x^"++(show k)++s) "" . getMap

constructPolynomial :: (Enum a, Num a) => [b] -> Polynomial a b
constructPolynomial = Polynomial . Map.fromDistinctAscList . zip [0..]

eval :: (Integral a, Num b) => Polynomial a b -> b -> b
eval p x = Map.foldrWithKey (\k a -> (x^k*a+)) 0 $ getMap p

polynomialDiv :: (Integral a, Show b, Show a, Eq b, Fractional b) =>
			Polynomial a b ->
			Polynomial a b ->
			(Polynomial a b, Polynomial a b)
polynomialDiv n d
	| n == 0 || degree n < degree d = (0, n)
	| otherwise = first (+t) $ polynomialDiv (n-t*d) d
	where t = traceShowId $ (lead n) `divideTerm` (lead d)

lead :: Polynomial a b -> (a, b)
lead = Map.findMax . getMap

degree :: Polynomial a b -> a
degree = fst . lead

divideTerm :: (Num a, Fractional b) => (a,b) -> (a,b) -> Polynomial a b
divideTerm (a,b) (c,d) = Polynomial $ Map.singleton (a-c) (b/d)