import Data.Char
import Data.Function.Memoize

squareDigits :: Int -> Int
squareDigits = sum . map ((^2) . digitToInt) . show

squareDigitChain :: Int -> Bool
squareDigitChain = memoFix squareDigitChain'

squareDigitChain' _ 1 = False
squareDigitChain' _ 89 = True
squareDigitChain' f n = f $ squareDigits n

main = print $ length $ filter squareDigitChain [1..10^6]