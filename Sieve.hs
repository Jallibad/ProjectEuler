{-# LANGUAGE TupleSections, FlexibleContexts #-}

module Sieve where

import Control.Monad
import Data.Array.IArray
import Data.Array.MArray

divisorSieve :: (Integral i, Ix i, IArray a e) => (e -> i -> e) -> e -> i -> a i e
divisorSieve f starter n = foldr addFactors emptyFactors [1..n]
	where
		emptyFactors = listArray (1,n) $ repeat starter
		addFactors x arr = accum f arr $ map (,x) [2*x,3*x..n]

totientSieve :: (MArray a e m, Ix e, Integral e) => e -> m (a e e)
totientSieve n = do
	arr <- newListArray (0,n) [0..]
	mapM_ (innerFold arr n) [2..n]
	return arr

innerFold :: (MArray a e m, Ix e, Integral e) => a e e -> e -> e -> m ()
innerFold arr n i = do
	a <- readArray arr i
	when (a == i) $
		mapM_ (adjust arr (\x -> x*(i-1) `div` i)) [i,2*i..n]

adjust :: (MArray a e m, Ix i) => a i e -> (e -> e) -> i -> m ()
adjust arr f i = readArray arr i >>= writeArray arr i . f

sieveOfAtkinWheel :: (MArray a Bool m, Ix i, Integral i) => i -> m (a i Bool)
sieveOfAtkinWheel limit = do
	let s = [1,7,11,13,17,19,23,29,31,37,41,43,47,49,53,59]
	newListArray (0,10) $ repeat False