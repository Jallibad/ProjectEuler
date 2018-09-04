import Data.Function.Memoize
import MathFunctions (digitSquare)

squareDigitChain :: Int -> Bool
squareDigitChain = memoFix squareDigitChain'

squareDigitChain' _ 1 = False
squareDigitChain' _ 89 = True
squareDigitChain' f n = f $ digitSquare n

main = print $ length $ filter squareDigitChain [1..10^6]