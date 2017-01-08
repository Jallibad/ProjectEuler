import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Data.Char

triangleNumbers = map (\n -> n*(n+1) `div` 2) [1..]

isTriangleWord :: String -> Bool
isTriangleWord word = number == (head $ dropWhile (<number) triangleNumbers)
	where number = sum $ map (\x -> ord x - ord 'A' + 1) word

main = readFile "Problem 42 Words.txt" >>= print . length . filter isTriangleWord . lines