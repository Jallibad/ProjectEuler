import Control.Arrow ((&&&))
import Data.List (mapAccumL)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)

tiles n = snd $ tiles' (Map.singleton 0 1) n
	where	tiles' m n
			| n < 0 = (m, 0)
			| otherwise = fromMaybe (Map.insert n ans m', ans) memoizePart
				where	memoizePart = fmap (const m &&& id) $ Map.lookup n m
					(m', a) = mapAccumL tiles' m [n-1, n-2, n-3, n-4]
					ans = sum a

main = print $ tiles 50