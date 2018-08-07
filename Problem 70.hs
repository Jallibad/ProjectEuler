{-# LANGUAGE TupleSections, BangPatterns #-}

import Control.Applicative ((<$>))
import Control.Monad (foldM)
import Control.Monad.ST (runST, ST)
import Data.Array.ST (readArray, STUArray)
import Data.Function (on)
import Data.List (minimumBy, sort)
import Sieve (totientSieve)

{-
--Taken from "http://programmingpraxis.com/2012/07/10/sieving-for-totients/" NOT MY WORK
totients :: Integral a => a -> [a]
totients n = M.elems $ foldl (\m i -> if m M.! i == i then foldr (M.adjust (\x -> x*(i-1) `div` i)) m [i,2*i..n] else m)
	(M.fromList $ zip [0..n] [0..n]) [2..n]
-}

minFun :: (Fractional a, Ord a) => (Int, a) -> (Int, Int) -> (Int, a)
minFun !a (!i,!e)
	| ((/=) `on` (sort . show)) i e || ((snd a) < b') = a
	| otherwise = (i, b')
	where b' = ((/) `on` fromIntegral) i e

new n = fst $ runST $ do
	arr <- totientSieve n :: ST s (STUArray s Int Int)
	foldM (minReduction arr) (100,100) [2..n]

minReduction arr curr i = (minFun curr . (i,)) <$> readArray arr i

main = print $ new $ 10^6