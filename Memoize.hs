{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Memoize where

import Data.Array.IArray
import qualified Data.Map.Strict as Map
import Data.Maybe

class MemoMap a i e where
	(?) :: a i e -> i -> Maybe e
	lookupFrom :: MemoMap a i e => (i -> e) -> a i e -> i -> e
	lookupFrom f s n = fromMaybe (f n) $ s ? n
	--get :: a i e -> i -> e

instance (Integral i, Ix i, IArray a e) => MemoMap a i e where
	s ? n
		| inRange (bounds s) n = Just $ s ! n
		| otherwise = Nothing

instance Ord i => MemoMap Map.Map i e where
	s ? n = Map.lookup n s