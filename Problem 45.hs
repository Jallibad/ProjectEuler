--Hexagonal numbers are also triangle numbers
pentagonals = map (\n -> n*(3*n-1) `div` 2) [1..]
hexagonals = map (\n -> n*(2*n-1)) [1..]

infElem :: Eq (a) => Ord (a) => a -> [a] -> Bool
n `infElem` (x:xs)
	| n == x = True
	| n < x = False
	| otherwise = infElem n xs

infIntersect :: Eq (a) => Ord (a) => [a] -> [a] -> [a]
infIntersect [] _ = []
infIntersect _ [] = []
infIntersect (x:xs) ys
	| x `infElem` ys = x : (infIntersect xs $ dropWhile (/= x) ys)
	| otherwise = infIntersect xs ys

main = print $ last $ take 3 $ infIntersect hexagonals pentagonals