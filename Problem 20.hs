import MathFunctions (factorial)
import Data.Char (digitToInt)
import Data.List
import Control.Arrow

digitSum :: (Integral a, Show a) => a -> Int
digitSum = sum . map digitToInt . show

main = print $ sum $ map digitToInt $ show $ factorial 100