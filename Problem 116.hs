import Control.Arrow ((&&&))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)

redTiles n = fst $ redTiles' n True Map.empty
	where	redTiles' n needTile m
			| n <= 0 = (if (n == 0) && (not needTile) then 1 else 0, m)
			| otherwise = fromMaybe (ans, Map.insert (n, needTile) ans m') memoizePart
				where	memoizePart = fmap (id &&& const m) $ Map.lookup (n, needTile) m
					(singleAns, singleM) = redTiles' (n-1) needTile m
					(tileAns, m') = redTiles' (n-2) False singleM
					ans = singleAns+tileAns

greenTiles n = fst $ greenTiles' n True Map.empty
	where	greenTiles' n needTile m
			| n <= 0 = (if (n == 0) && (not needTile) then 1 else 0, m)
			| otherwise = fromMaybe (ans, Map.insert (n, needTile) ans m') memoizePart
				where	memoizePart = fmap (id &&& const m) $ Map.lookup (n, needTile) m
					(singleAns, singleM) = greenTiles' (n-1) needTile m
					(tileAns, m') = greenTiles' (n-3) False singleM
					ans = singleAns+tileAns

blueTiles n = fst $ blueTiles' n True Map.empty
	where	blueTiles' n needTile m
			| n <= 0 = (if (n == 0) && (not needTile) then 1 else 0, m)
			| otherwise = fromMaybe (ans, Map.insert (n, needTile) ans m') memoizePart
				where	memoizePart = fmap (id &&& const m) $ Map.lookup (n, needTile) m
					(singleAns, singleM) = blueTiles' (n-1) needTile m
					(tileAns, m') = blueTiles' (n-4) False singleM
					ans = singleAns+tileAns

main = print $ (redTiles 50) + (greenTiles 50) + (blueTiles 50)