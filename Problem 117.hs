import qualified Data.Map.Strict as Map

tiles n = fst $ tiles' n $ Map.singleton 0 1
	where	tiles' n m
			| n < 0 = (0, m)
			| n `Map.member` m = (m Map.! n, m)
			| otherwise = (ans, Map.insert n ans $ snd $ last xs)
				where	xs = scanl (\(_, m') x -> tiles' x m') (0, m) [n-1, n-2, n-3, n-4]
					ans = sum $ map fst xs

main = print $ tiles 50