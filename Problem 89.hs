import Control.Arrow ((&&&))
import Data.Function (on)
import RomanNumeral

charSaved :: RomanNumeral -> Int
charSaved = uncurry (on (-) $ length . show) . (id &&& simplify)

main = readFile "Problem 89 Roman Numerals.txt" >>= print . sum . map (charSaved . RomanNumeral) . lines