import qualified Data.Map.Strict as Map
import Data.Maybe

coinSums n = fst $ coinSums' n 200 Map.empty
	where	coinSums' 0 _ m = (1, m)
		coinSums' n max m = fromMaybe (ans, Map.insert (n, max) ans m'') $ fmap (\x -> (x, m)) $ Map.lookup (n, max) m
			where	(ans, m'') = foldl (\(prev, m') coin -> let (a, b)=coinSums' (n-coin) coin m' in (prev+a, b)) (0, m) $ takeWhile (\i -> i<=n && i<=max) [1,2,5,10,20,50,100,200]

main = print $ coinSums 200