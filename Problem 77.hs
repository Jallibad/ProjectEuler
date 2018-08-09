{-# LANGUAGE TupleSections, ScopedTypeVariables, ViewPatterns, MultiParamTypeClasses, FlexibleInstances, TypeFamilies, InstanceSigs #-}

import Control.Arrow (first, second)
import Control.Monad.Fix (fix)
import Control.Monad.ST
import Data.STRef
import Data.List (mapAccumL)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import MathFunctions

class Memoizer m a b where
	--type MemoizingMap = m a b
	--lifter :: (a -> b)
	insertResult :: (a -> m a b -> (m a b, b)) -> (a -> m a b -> (m a b, b))
	memoizeLookup :: a -> m a b -> Maybe (m a b, b)
	empty :: m a b
	memoizeCall :: (a -> m a b -> (m a b, b)) -> (a -> m a b -> (m a b, b))
	memoizeCall (insertResult -> f) input map = fromMaybe (f input map) $ memoizeLookup input map

type FunctionInput = (Integer, Integer)
type FunctionOutput = Integer
type Function = (FunctionInput -> FunctionOutput) -> FunctionInput -> FunctionOutput
type MemoizingMap' = Map.Map FunctionInput FunctionOutput
type MemoizingResult = (MemoizingMap', FunctionOutput)
type MemoizingFunction = FunctionInput -> MemoizingMap' -> MemoizingResult

lifter :: (FunctionInput -> FunctionOutput) -> MemoizingFunction
lifter f input map = (Map.insert input output map, output)
	where
		output = f input

lifter' :: Function -> MemoizingFunction -> MemoizingFunction
lifter' (f :: Function) (memoizeCall -> g :: MemoizingFunction) input map = (Map.insert input output map, output)
	where
		output = f (snd . flip g map) input

instance (Ord a) => Memoizer Map.Map a b where
	insertResult :: (a -> Map.Map a b -> (Map.Map a b, b)) -> (a -> Map.Map a b -> (Map.Map a b, b))
	insertResult f input map = first (Map.insert input $ snd result) result
		where
			result = f input map
	memoizeLookup input map = fmap (map,) $ Map.lookup input map
	empty = Map.empty

insertResult' :: MemoizingFunction -> MemoizingFunction
insertResult' f input map = first (Map.insert input $ snd result) result
	where
		result = f input map

memoizeLookup' :: FunctionInput -> MemoizingMap' -> Maybe MemoizingResult
memoizeLookup' input map = fmap (map,) $ Map.lookup input map

primeSums :: MemoizingFunction -> MemoizingFunction
primeSums _ (0, _) m = (m, 1)
primeSums (memoizeCall -> f) (n, max) m = foldl addPrime (m,0) $ takeWhile (<=(min n max)) primes
	where
		addPrime (m, p) x = second (+p) $ f ((n-x), (min x (n-x))) m

memoizeStart f input = snd $ memoizeCall (fix (insertResult . f)) input empty

primeSums' :: Function
primeSums' _ (0, _) = 1
primeSums' (f :: FunctionInput -> FunctionOutput) (n, max) = sum $ map addPrime $ takeWhile (<=(min n max)) primes
	where
		addPrime x = f (n-x, min x $ n-x)

main = print $ until ((>5000) . snd . (\n -> (fix primeSums) (n, (n-1)) Map.empty)) (+1) 1