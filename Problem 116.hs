import Data.Function.Memoize (memoFix3)

tiles :: Int -> Int -> Int
tiles = memoFix3 tiles' True

redTiles = tiles 2
greenTiles = tiles 3
blueTiles = tiles 4

tiles' :: (Num a, Ord a, Num b) => (Bool -> a -> a -> b) -> Bool -> a -> a -> b
tiles' _ False _ 0 = 1
tiles' f needTile len n
	| n <= 0 = 0
	| otherwise = (f needTile len $ n-1) + (f False len $ n-len)

main = print $ (redTiles 50) + (greenTiles 50) + (blueTiles 50)