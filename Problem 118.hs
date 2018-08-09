import MathFunctions (isPrime)
import Data.List
import Data.Char (intToDigit)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import Control.Arrow ((&&&))
import Debug.Trace

intListToInt :: [Int] -> Int
intListToInt = read . map intToDigit

--primeSet f [] = [f]
--primeSet f xs = concat [primeSet (a:f) b | x <- [1..length xs], let (a',b) = splitAt x xs, let a = intListToInt a', ((null f) || a > (head f)) && isPrime a]

--primeSet _ [] = 1
--primeSet f xs = sum [primeSet a b | x <- [1..length xs], let (a',b) = splitAt x xs, let a = intListToInt a', a > f && isPrime a]

primeSet' m _ [] = (m, 1)
primeSet' m f xs = fromMaybe (Map.insert (f, xs) ans m'', ans) memoizePart
	where	memoizePart = fmap (const m &&& id) $ Map.lookup (f, xs) m
		(m'', ans) = fmap sum $ mapAccumL (\m' (a,b) -> primeSet' m' a b) m [(a,b) | x <- [1..length xs], let (a',b) = splitAt x xs, let a = intListToInt a', a > f && isPrime a]

primeSet :: Map.Map (Int, [Int]) Int -> Int -> [Int] -> (Map.Map (Int, [Int]) Int, Int)
primeSet m _ [] = (m, 1)
primeSet m f xs
	| (f, xs) `Map.member` m = (m, m Map.! (f, xs))
	| otherwise = (Map.insert (f, xs) ans m'', ans)
	where (m'', ans) = fmap sum $ mapAccumL (\m' (a,b) -> primeSet m' a b) m [(a,b) | x <- [1..length xs], let (a',b) = splitAt x xs, let a = intListToInt a', a > f && isPrime a]

main = print $ sum $ snd $ mapAccumL (\m i -> primeSet m 0 i) Map.empty $ permutations [1..9]