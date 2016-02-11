import ListFunctions
import Data.List
import Control.Applicative

--f :: [Char] -> [[(Int, Int)]]
f n = map (\x -> f1 x n) [1..7]

f1 :: Int -> [Char] -> (Int, Int)
f1 x n = head $ dropWhile (\(first, second) -> first < second) $ map (\x -> let (t, h)=splitAt x s in ((read f)*(read t), read h)) [1..length s - 1]
	where (f, s) = splitAt x n

main = print $ sum $ nub $ concatMap (map snd . filter (\(f, s) -> f == s)) $ map f $ permutations ['1'..'9']