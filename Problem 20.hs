import MathFunctions (factorial)
import Data.Char (digitToInt)

main = print $ sum $ map digitToInt $ show $ factorial 100