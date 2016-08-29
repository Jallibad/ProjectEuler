import Data.Bits (xor)
import Data.Char
import Data.List.Split (splitOn)

keys = [[a,b,c] | a <- alphabet, b <- alphabet, c <- alphabet]
	where alphabet = [ord 'a'.. ord 'z']

display :: [Int] -> String
display chars = map (\code -> let char = chr code in if any ($ char) [isSeparator, isLetter, isNumber] then char else '_') chars

valid :: [Int] -> Bool
valid chars = a && b
	where	a = all (isPrint . chr) chars
		b = all (\x -> all ($ (chr x)) [(/='`'), (/='&')]) chars

decrypt :: [Int] -> [Int] -> [Int]
decrypt message key = zipWith xor message $ cycle key

main = do
	file <- readFile "Problem 59 Cipher.txt"
	let encrypted = map read $ splitOn "," file
	print $ sum $ head $ filter valid $ map (decrypt encrypted) keys