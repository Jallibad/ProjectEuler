import Data.Char (ord)
import Data.List (sort)

nameScore :: Int -> String -> Int
nameScore rank name = rank*(placeInAlphabet name)
    where placeInAlphabet = sum . map (\x -> ord x - ord 'A' + 1)

main = readFile "Problem 22 Names Scores.txt" >>= print . sum . zipWith nameScore [1..] . sort . lines