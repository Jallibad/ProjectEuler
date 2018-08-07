{-# LANGUAGE TupleSections, FlexibleInstances #-}

module Sieve where

import Data.Array.IArray

divisorSieve :: (Integral i, Ix i, IArray a e) => (e -> i -> e) -> e -> i -> a i e
divisorSieve f starter n = foldr addFactors emptyFactors [1..n]
	where
		emptyFactors = listArray (1,n) $ repeat starter
		addFactors x arr = accum f arr $ map (,x) [2*x,3*x..n]