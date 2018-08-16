import ListFunctions (takeLast)
import Data.List ((\\))

singles = ['0'..'9']

threeDigDivBy :: Int -> [String]
threeDigDivBy n = filter (\x -> read x `mod` n == 0) [[a,b,c] | a <- singles, b <- singles, c <- singles, a/=b && a/=c && b/=c]

subStringDiv :: Int -> [String] -> [String]
--subStringDiv prime originalList = concatMap (\part -> [part++[last list] | list <- threeDigDivBy prime, (takeLast 2 part) == (take 2 list), null $ (last list : part) \\ singles]) originalList
subStringDiv prime originalList = [part++[last list] | part <- originalList, list <- threeDigDivBy prime, (takeLast 2 part) == (take 2 list), null $ (last list : part) \\ singles]

numberStart :: [String]
numberStart = [[a,b] | a <- singles, b <- singles, a/=b]

subStrings = map (subStringDiv $) [2,3,5,7,11,13,17]

main = print $ sum $ map (\s -> read $ (singles \\ s) ++ s) $ foldr ($) numberStart $ reverse subStrings