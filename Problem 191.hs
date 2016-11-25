import Control.Arrow ((&&&))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)

f number = fst $ f' number 0 False Map.empty
	where	f' 0 _ _ m = (1, m)
		f' n a l m = fromMaybe (ans, Map.insert (n, a, l) ans aMap) memoizePart
			where	memoizePart = fmap (id &&& const m) $ Map.lookup (n, a, l) m
				(lPath, lMap) = if l then (0, m) else f' (n-1) 0 True m
				(oPath, oMap) = f' (n-1) 0 l lMap
				(aPath, aMap) = if (a==2) then (0, oMap) else f' (n-1) (a+1) l oMap
				ans = lPath+oPath+aPath

main = print $ f 30