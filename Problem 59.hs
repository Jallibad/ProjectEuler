import Data.List
import Data.List.Split
import Data.Char
import Data.Bits

keys = [[a,b,c] | a <- alphabet, b <- alphabet, c <- alphabet]
	where alphabet = [ord 'a'.. ord 'z']

display :: [Int] -> String
display chars = map (\code -> let char = chr code in if or $ map ($ char) [isSeparator, isLetter, isNumber] then char else '_') chars

decrypt :: [Int] -> [Int] -> [Int]
decrypt message key = zipWith xor message $ cycle key
main = do
	file <- readFile "Problem 59 Cipher.txt"
	print $ length $ filter (\list -> "the" `isInfixOf` list) $ map display $ zipWith decrypt (repeat $ map read $ splitOn "," file) keys