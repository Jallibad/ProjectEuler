import MathFunctions (isPrime)
import Data.Function (on)

twiceSquares = map ((*2) . (^2)) [1..]

canBeWritten n = any isPrime $ map (n-) $ takeWhile (<n) twiceSquares

main = print $ head $ filter (\x -> (not $ isPrime x) && (not $ canBeWritten x)) [3,5..]