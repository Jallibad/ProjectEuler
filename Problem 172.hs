{-# LANGUAGE TupleSections #-}

import MathFunctions

import Control.Arrow
import qualified Data.Map as Map
import Data.Maybe
import Data.Array
import Data.List
import Data.Tuple

countDigitsNoLeadingZero :: Integer -> Integer
countDigitsNoLeadingZero n = sum $ snd $ mapAccumL (\memoize i -> swap $ countDigits n (counterUpdate threesList i) memoize) Map.empty [1..9]
	where threesList = listArray (0,9) $ repeat 3

type DigitCounter = Array Int Int
type MemoizingMap = Map.Map (Integer, DigitCounter) Integer

countDigits :: Integer -> DigitCounter -> MemoizingMap -> (Integer, MemoizingMap)
countDigits 0 _ memoize = (1, memoize)
countDigits n m memoize = maybe (ans, Map.insert (n, m) ans m') (,memoize) $ Map.lookup (n, m) memoize
	where
		updateFunc (p, memoize') i
			| m ! i > 0 = first (+p) $ countDigits (n-1) (counterUpdate m i) memoize'
			| otherwise = (p, memoize')
		(ans, m') = foldl' updateFunc (0, memoize) [0..9]

counterUpdate :: DigitCounter -> Int -> DigitCounter
counterUpdate arr i = arr // [(i, (arr ! i)-1)]

main = print $ countDigitsNoLeadingZero 17