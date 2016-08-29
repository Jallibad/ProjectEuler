import ListFunctions
import Control.Applicative
import Control.Arrow
import Data.List

f :: [Char] -> [(Int, Int)]
f n = map (f1 n) [1..7]
--f n = [ | x <- [1..7], let (multiplicand, s) = splitAt x n, y <- [1..length s - 1], let (multiplier, product) = splitAt y s]

f1 :: [Char] -> Int -> (Int, Int)
f1 n x = head $ dropWhile (uncurry (<)) [((read f)*(read t), read h) | x <- [1..length s - 1], let (t, h) = splitAt x s]
	where (f, s) = splitAt x n

f2 :: [Char] -> Int -> [(Int, Int)]
f2 n x = [((read f)*(read t), read h) | x <- [1..length s - 1], let (t, h) = splitAt x s]
	where (f, s) = splitAt x n

main = print $ sum $ nub $ concatMap (map snd . filter (uncurry (==))) $ map f $ permutations ['1'..'9']