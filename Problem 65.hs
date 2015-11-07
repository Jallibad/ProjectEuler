import Data.List
import Data.Char

infiniteA :: [Integer]
infiniteA = repeat 1

infiniteB :: [Integer]
infiniteB = 2 : 1 : (intercalate [1,1] $ map (:[]) [2,4..])

formula :: [Integer] -> [Integer]
formula list = zipWith (+) listA listB
	where
		listA = zipWith (*) (drop 2 infiniteB) (tail list)
		listB = zipWith (*) (drop 2 infiniteA) list

convergentA :: [Integer]
convergentA = 2 : 3 : (formula convergentA)

main = print $ sum $ map digitToInt $ show $ convergentA !! (100-1)