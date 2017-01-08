import Control.Arrow ((&&&))
import Data.List (mapAccumL)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)

tiles x n m
	| n < x = (m, 1)
	| otherwise = fromMaybe (Map.insert n ans m'', ans) memoizePart
		where	memoizePart = fmap (const m &&& id) $ Map.lookup n m
			(m'', a) = mapAccumL (\m' i -> tiles x (if i < n then n-i-1 else n-i) m') m $ 0:[x..n]
			ans = sum a

main = print $ fst $ head $ dropWhile ((<10^6) . snd) $ zip [50..] $ snd $ mapAccumL (flip $ tiles 50) Map.empty [50..]