import Data.List ((\\))
import Data.Char (digitToInt)
import MathFunctions (isqrt)

digitSum :: [Char] -> Int
digitSum s = sum $ map digitToInt s

numDigits :: Integer -> Int -> [Char]
numDigits n d = head $ dropWhile ((<d) . length) $ map (\x -> show $ isqrt (n*10^(2*x))) [0..]

nonSquares :: [Integer]
nonSquares = [1..100] \\ (takeWhile (<=100) $ map (^2) [1..])

main = print $ sum $ map (\x -> digitSum $ numDigits x 100) nonSquares