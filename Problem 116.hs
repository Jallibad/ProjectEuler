import qualified Data.Map.Strict as Map

redTiles n = fst $ redTiles' n True Map.empty
	where	redTiles' n needTile m
			| n <= 0 = (if (n == 0) && (not needTile) then 1 else 0, m)
			| (n, needTile) `Map.member` m = (m Map.! (n, needTile), m)
			| otherwise = (ans, Map.insert (n, needTile) ans m')
				where	(singleAns, singleM) = redTiles' (n-1) needTile m
					(tileAns, m') = redTiles' (n-2) False singleM
					ans = singleAns+tileAns

greenTiles n = fst $ greenTiles' n True Map.empty
	where	greenTiles' n needTile m
			| n <= 0 = (if (n == 0) && (not needTile) then 1 else 0, m)
			| (n, needTile) `Map.member` m = (m Map.! (n, needTile), m)
			| otherwise = (ans, Map.insert (n, needTile) ans m')
				where	(singleAns, singleM) = greenTiles' (n-1) needTile m
					(tileAns, m') = greenTiles' (n-3) False singleM
					ans = singleAns+tileAns

blueTiles n = fst $ blueTiles' n True Map.empty
	where	blueTiles' n needTile m
			| n <= 0 = (if (n == 0) && (not needTile) then 1 else 0, m)
			| (n, needTile) `Map.member` m = (m Map.! (n, needTile), m)
			| otherwise = (ans, Map.insert (n, needTile) ans m')
				where	(singleAns, singleM) = blueTiles' (n-1) needTile m
					(tileAns, m') = blueTiles' (n-4) False singleM
					ans = singleAns+tileAns

main = print $ (redTiles 50) + (greenTiles 50) + (blueTiles 50)