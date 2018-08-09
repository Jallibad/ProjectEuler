import Control.Arrow
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Ord

collatz :: Int -> Map.Map Int Int -> (Int, Map.Map Int Int)
collatz 1 m = (1, m)
collatz n m = fromMaybe (ans+1, Map.insert n (ans+1) m') $ fmap (id &&& const m) $ Map.lookup n m
	where (ans, m') = collatz (if n `mod` 2 == 0 then n `div` 2 else 3*n+1) m

main = print $ (\(x, _, _) -> x) $ foldl (\(a, b, m) x -> let (y,m')=(collatz x m) in if y>b then (x, y, m') else (a, b, m')) (0, 0, Map.empty) [1..10^6-1]