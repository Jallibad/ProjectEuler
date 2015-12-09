import Data.List

union' :: Eq a => [a] -> [a] -> [a]
union' x [] = x
union' [] y = y
union' (x:xs) (y:ys)
	| x==y = x : (union' xs ys)
	| otherwise = x : (union' xs (y:ys))

contains :: Eq (a) => [a] -> [a] -> Bool
_ `contains` [] = True
[] `contains` _ = False
(l:list) `contains` (x:xs)
	| l == x = list `contains` xs
	| otherwise = list `contains` (x:xs)

--main = readFile "Problem 79 Key Log.txt" >>= print . nub . lines

--foldl1 union' x