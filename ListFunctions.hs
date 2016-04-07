module ListFunctions where

import Data.List (inits, tails)
import qualified Data.Array as Array (indices, elems, listArray, (//), (!))
import Data.Maybe (fromJust)

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil f (x:xs) = case (f x) of
				True -> [x]
				False -> x : takeUntil f xs

takeLast :: Int -> [a] -> [a]
takeLast number list = drop (length list - number) list

isPalindrome :: Eq (a) => [a] -> Bool
isPalindrome xs = first == second
	where	half = (length xs) `div` 2
		first = take half xs
		second = take half $ reverse xs

--Taken from Data.List.Ordered
orderedUnion :: Ord a => [a] -> [a] -> [a]
orderedUnion = loop
  where
     loop [] ys = ys
     loop xs [] = xs
     loop (x:xs) (y:ys)
       = case compare x y of
          LT -> x : loop xs (y:ys)
          EQ -> x : loop xs ys
          GT -> y : loop (x:xs) ys

differences [] = []
differences (_:[]) = []
differences (x:y:xs) = (y-x) : differences (y:xs)

allEqual :: Eq (a) => [a] -> Bool
allEqual [] = True
allEqual (x:xs) = all (==x) xs

pick :: Int -> [a] -> [[a]]
pick 0 _ = [[]]
pick _ [] = []
pick n list@(x:xs) = (map (x:) $ pick (n-1) xs)++(pick n xs)

circulateList :: [a] -> [[a]]
circulateList list = tail $ zipWith (++) (tails list) (inits list)

orderedPermutations :: Ord (a) => [a] -> [[a]]
orderedPermutations xs = xs : (nextPermutation $ Array.listArray (0, n) xs)
	where	nextPermutation a
			| null kList = []
			| otherwise = answer : (nextPermutation $ Array.listArray (0, n) answer)
			where	kList = filter (\x -> (a Array.! x)<(a Array.! (x+1))) [0..n-1]
				k = last kList
				l = last $ filter (\x -> (a Array.! k)<(a Array.! x)) $ dropWhile (<=k) [0..n]
				swapped = (a Array.// [(k, a Array.! l), (l, a Array.! k)])
				answer = (take (k+1) $ Array.elems swapped)++(reverse $ drop (k+1) $ Array.elems swapped)
		n = length xs - 1

takePairs :: [a] -> [(a,a)]
takePairs [] = []
takePairs (_:[]) = []
takePairs (x1:x2:xs) = (x1,x2) : takePairs (x2:xs)