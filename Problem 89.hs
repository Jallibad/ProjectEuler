import RomanNumeral

charSaved :: RomanNumeral -> Int
charSaved numeral = (length $ show numeral) - (length $ show $ simplify numeral)

main = readFile "Problem 89 Roman Numerals.txt" >>= print . sum . map (charSaved . RomanNumeral) . lines