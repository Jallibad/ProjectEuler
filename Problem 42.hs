import Data.Maybe
import Data.List
triangleNumbers = map (\n -> n*(n+1) `div` 2) [1..]

isTriangleWord :: String -> Bool
isTriangleWord word = number == (head $ dropWhile (<number) triangleNumbers)
	where number = sum $ map (\x -> (fromJust $ x `elemIndex` ['A'..'Z']) + 1) word

main = do
	file <- readFile "Problem 42 Words.txt"
	print $ length $ filter isTriangleWord $ lines file