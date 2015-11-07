toRomanNumeral :: Int -> String
toRomanNumeral 0 = ""
toRomanNumeral 4 = "IV"
toRomanNumeral 9 = "IX"
toRomanNumeral number
	| 40 <= number && number < 50 = "XL" ++ (toRomanNumeral (number-40))
	| 90 <= number && number < 100 = "XC" ++ (toRomanNumeral (number-90))
	| 400 <= number && number < 500 = "CD" ++ (toRomanNumeral (number-400))
	| 900 <= number && number < 1000 = "CM" ++ (toRomanNumeral (number-900))
	| otherwise = nextNumeral : (toRomanNumeral (number-nextValue))
	where (nextNumeral, nextValue) = head $ filter (\(_, n) -> n <= number) [('M',1000),('D',500),('C',100),('L',50),('X',10),('V',5),('I',1)]

fromRomanNumeral :: String -> Int
fromRomanNumeral "" = 0
fromRomanNumeral ('I':'X':rest) = 9 + (fromRomanNumeral rest)
fromRomanNumeral ('I':'V':rest) = 4 + (fromRomanNumeral rest)
fromRomanNumeral ('I':rest) = 1 + (fromRomanNumeral rest)
fromRomanNumeral ('V':rest) = 5 + (fromRomanNumeral rest)
fromRomanNumeral ('X':'C':rest) = 90 + (fromRomanNumeral rest)
fromRomanNumeral ('X':'L':rest) = 40 + (fromRomanNumeral rest)
fromRomanNumeral ('X':rest) = 10 + (fromRomanNumeral rest)
fromRomanNumeral ('L':rest) = 50 + (fromRomanNumeral rest)
fromRomanNumeral ('C':'M':rest) = 900 + (fromRomanNumeral rest)
fromRomanNumeral ('C':'D':rest) = 400 + (fromRomanNumeral rest)
fromRomanNumeral ('C':rest) = 100 + (fromRomanNumeral rest)
fromRomanNumeral ('D':rest) = 500 + (fromRomanNumeral rest)
fromRomanNumeral ('M':rest) = 1000 + (fromRomanNumeral rest)
fromRomanNumeral _ = error "not a valid roman numeral"

charSaved :: String -> Int
charSaved numeral = (length numeral)-(length $ toRomanNumeral $ fromRomanNumeral numeral)

main = do
	file <- readFile "Problem 89 Roman Numerals.txt"
	print $ sum $ map charSaved $ lines file