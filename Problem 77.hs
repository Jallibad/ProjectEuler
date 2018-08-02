{-# LANGUAGE TupleSections, ScopedTypeVariables, ViewPatterns, MultiParamTypeClasses, FlexibleInstances, TypeFamilies #-}

import Control.Arrow (first, second)
import Control.Monad.Fix (fix)
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
	--memoizeCall (insertResult -> f) input map = fromMaybe (f input map) $ memoizeLookup input map

type FunctionInput = (Integer, Integer)
type FunctionOutput = Integer
type MemoizingMap' = Map.Map FunctionInput FunctionOutput
type MemoizingResult = (MemoizingMap', FunctionOutput)
type MemoizingFunction = FunctionInput -> MemoizingMap' -> MemoizingResult

lifter :: (FunctionInput -> FunctionOutput) -> MemoizingFunction
lifter f input map = (Map.insert input output map, output)
	where
		output = f input

--lifter' :: ((FunctionInput -> FunctionOutput) -> FunctionInput -> FunctionOutput) -> (MemoizingFunction -> MemoizingFunction)
--lifter'

instance (Ord a) => Memoizer Map.Map a b where
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

memoizeCall' :: MemoizingFunction -> MemoizingFunction
memoizeCall' (insertResult' -> f) input map = fromMaybe (f input map) $ memoizeLookup' input map

primeSums :: MemoizingFunction
primeSums (0, _) m = (m, 1)
primeSums (n, max) m = foldl addPrime (m,0) $ takeWhile (<=(min n max)) primes
	where
		addPrime (m, p) x = second (+p) $ memoizeCall' primeSums ((n-x), (min x (n-x))) m

memoizeStart f input = snd $ memoizeCall (fix (insertResult . f)) input empty

primeSums' :: (FunctionInput -> FunctionOutput) -> (FunctionInput -> FunctionOutput)
primeSums' _ (0, _) = 1
primeSums' f (n, max) = foldl addPrime 0 $ takeWhile (<=(min n max)) primes
	where
		addPrime p x = p+(f (n-x, min x (n-x)))

main = print $ until ((>5000) . snd . (\n -> memoizeCall' primeSums (n, (n-1)) Map.empty)) (+1) 1