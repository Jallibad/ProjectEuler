{-# LANGUAGE ViewPatterns #-}

import Data.Bits (xor)
import Data.Char
import Data.List.Split (splitOn)

keys = [[a,b,c] | a <- alphabet, b <- alphabet, c <- alphabet]
	where alphabet = [ord 'a'.. ord 'z']

display :: [Int] -> String
display = map $ \(chr -> char) -> if any ($ char) [isSeparator, isLetter, isNumber] then char else '_'

valid :: [Int] -> Bool
valid = all $ \(chr -> x) -> isPrint x && x/='`'

decrypt :: [Int] -> [Int] -> [Int]
decrypt message = zipWith xor message . cycle

main = do
	file <- readFile "Problem 59 Cipher.txt"
	let encrypted = map read $ splitOn "," file
	print $ sum $ head $ filter valid $ map (decrypt encrypted) keys