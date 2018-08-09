{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Memoize where

import Data.STRef
import Control.Monad.ST
import Data.Array.IArray
import qualified Data.Map.Strict as Map
import Data.Maybe

class MemoMap a i e where
	(?) :: a i e -> i -> Maybe e
	update :: i -> e -> a i e -> a i e
	lookupFrom :: MemoMap a i e => (i -> e) -> a i e -> i -> e
	lookupFrom f s n = fromMaybe (f n) $ s ? n
	--get :: a i e -> i -> e

instance (Integral i, Ix i, IArray a e) => MemoMap a i e where
	s ? n
		| inRange (bounds s) n = Just $ s ! n
		| otherwise = Nothing
	--update s i o = 

instance Ord i => MemoMap Map.Map i e where
	m ? n = Map.lookup n m
	update i o m = Map.insert i o m

--memoizeHelper :: MemoMap a i e => (STRef s (a i e) -> i -> ST s e) -> (STRef s (a i e) -> i -> ST s e)
memoizeHelper :: Ord i => (STRef s (Map.Map i e) -> i -> ST s e) -> (STRef s (Map.Map i e) -> i -> ST s e)
memoizeHelper f ref input = memoizeLookup input ref >>= memoizeCall (f ref) input ref

--memoizeLookup :: MemoMap a i e => i -> STRef s (a i e) -> ST s (Maybe e)
memoizeLookup :: Ord i => i -> STRef s (Map.Map i e) -> ST s (Maybe e)
memoizeLookup input = fmap (Map.lookup input) . readSTRef

--memoizeCall :: MemoMap a i e => (i -> ST s e) -> i -> STRef s (a i e) -> Maybe e -> ST s e
memoizeCall :: Ord i => (i -> ST s e) -> i -> STRef s (Map.Map i e) -> Maybe e -> ST s e
memoizeCall f input ref = fromMaybe (do
	output <- f input
	modifySTRef ref $ Map.insert input output
	return output
	) . fmap return