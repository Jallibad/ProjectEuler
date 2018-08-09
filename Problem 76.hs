import Control.Arrow ((&&&))
import Data.List (mapAccumL)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)

countSums n = snd $ countSums' n (n-1) Map.empty
	where	countSums' 0 _ m = (m, 1)
		countSums' n max m = fromMaybe (Map.insert (n, max) ans m'', ans) memoizePart
			where	memoizePart = fmap (const m &&& id) $ Map.lookup (n, max) m
				(m'', y) = mapAccumL (\m' x -> countSums' (n-x) (min x (n-x)) m') m [1.. min n max]
				ans = sum y

main = print $ countSums 100