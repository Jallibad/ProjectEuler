{-# LANGUAGE TupleSections #-}

import qualified Data.Map.Strict as Map
import Control.Monad
import Control.Applicative
import Control.Arrow
import Data.Maybe

f n = fst $ f' (n+1) Map.empty

f' :: Integer -> Map.Map Integer Integer -> (Integer, Map.Map Integer Integer)
f' 0 m = (0, m)
f' 1 m = (1, m)
f' n m = maybe (ans, Map.insert n ans m'') (,m) $ Map.lookup n m
	where
		halfN = n `div` 2
		(ans1, m') = f' halfN m
		(ans2, m'') = if odd n then (f' (halfN+1) m') else (0, m')
		ans = ans1 + ans2

main = print $ f $ 10^25