import Data.List (elemIndex, sort)
import Data.Maybe (fromJust)

nameScore :: String -> Int
nameScore name = sum $ map (\x -> (fromJust $ x `elemIndex` ['A'..'Z']) + 1) name

main = readFile "Problem 22 Names Scores.txt" >>= print . sum . zipWith (*) [1..] . map nameScore . sort . lines