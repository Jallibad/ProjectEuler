import qualified Data.Map.Strict as Map

tiles n = fst $ tiles' n $ Map.empty
	where	tiles' n m
			| n < 3 = (1, m)
			| n `Map.member` m = (m Map.! n, m)
			| otherwise = (ans, Map.insert n ans $ snd $ last xs)
				where	(singleAns, singleM) = tiles' (n-1) m
					xs = scanl (\(_, m') i -> tiles' (if i < n then n-i-1 else n-i) m') (0, singleM) [3..n]
					ans = (sum $ map fst xs)+singleAns

main = print $ tiles 50