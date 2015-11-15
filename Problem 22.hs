import Data.List
import Data.Maybe

nameScore :: String -> Int
nameScore name = sum $ map (\x -> (fromJust $ x `elemIndex` ['A'..'Z']) + 1) name

main = do
	file <- readFile "Problem 22 Names Scores.txt"
	print $ sum $ zipWith (*) [1..] $ map nameScore $ sort $ lines file