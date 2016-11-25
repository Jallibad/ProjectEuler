import Control.Arrow ((&&&))
import Data.List (mapAccumL)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)

tiles n = snd $ tiles' n $ Map.empty
	where	tiles' n m
			| n < 3 = (m, 1)
			| otherwise = fromMaybe (Map.insert n ans m'', ans) memoizePart
				where	memoizePart = fmap (const m &&& id) $ Map.lookup n m
					(m'', a) = mapAccumL (\m' i -> tiles' (if i < n then n-i-1 else n-i) m') m $ 0:[3..n]
					ans = sum a

main = print $ tiles 50