{-# LANGUAGE TupleSections, ScopedTypeVariables, ViewPatterns, FlexibleContexts #-}

import qualified Data.Map.Strict as Map
import Data.Maybe
import Control.Arrow
import Data.STRef
import Control.Monad
import Control.Monad.ST
import Control.Monad.Fix (fix)
import Memoize

coinSums n = runST $ newSTRef Map.empty >>= \ref -> coinSumsM ref (n, 200)

coinSumsM :: STRef s (Map.Map (Int, Int) Int) -> (Int, Int) -> ST s Int
coinSumsM _ (0, _) = return 1
coinSumsM ref (n, max) = fmap sum $ mapM (\coin -> memoizeHelper coinSumsM ref ((n-coin), coin)) $ takeWhile (\i -> i<=n && i<=max) [1,2,5,10,20,50,100,200]

main = print $ coinSums 200