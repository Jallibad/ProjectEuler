import Data.List
import Data.List.Split
import Data.Char
import Data.Bits

keys = [[a,b,c] | a <- alphabet, b <- alphabet, c <- alphabet]
	where alphabet = [ord 'a'.. ord 'z']

display :: [Int] -> String
display chars = map (\code -> let char = chr code in if or $ map ($ char) [isSeparator, isLetter, isNumber] then char else '_') chars

valid :: [Int] -> String
valid = all (\c -> )

decrypt :: [Int] -> [Int] -> [Int]
decrypt message key = zipWith xor message $ cycle key

main = do
	file <- readFile "Problem 59 Cipher.txt"
	let encrypted = map read $ splitOn "," file
	print $ head $ map (map chr) $ filter valid $ map (decrypt encrypted) keys