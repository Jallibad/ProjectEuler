import qualified Data.Map.Strict as Map

f number = fst $ f' number 0 False Map.empty
	where	f' 0 _ _ m = (1, m)
		f' n a l m
			| (n, a, l) `Map.member` m = (m Map.! (n, a, l), m)
			| otherwise = (ans, Map.insert (n, a, l) ans aMap)
				where	(lPath, lMap) = if l then (0, m) else f' (n-1) 0 True m
					(oPath, oMap) = f' (n-1) 0 l lMap
					(aPath, aMap) = if (a==2) then (0, oMap) else f' (n-1) (a+1) l oMap
					ans = lPath+oPath+aPath

main = print $ f 30