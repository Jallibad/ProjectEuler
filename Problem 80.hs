import Data.List
import Data.Char
import Data.Ord

--This doesn't work when it gets to a digit five or greater in the decimal
nextDigit :: Integer -> Integer -> Integer
nextDigit power current = minimumBy (comparing (\x -> abs $ aim-(x^2))) choices
	where	choices = map (current*10+) [0..9]
		aim = power*10^((length $ show $ (head choices)^2)-1)

isqrt :: Integral (a) => a -> a
isqrt 0 = 0
isqrt 1 = 1
isqrt n = head $ dropWhile (\x -> x*x > n) $ iterate (\x -> (x + n `div` x) `div` 2) (n `div` 2)

splitTwos :: [a] -> [[a]]
splitTwos [] = [[]]
splitTwos (x:y:[]) = [[x,y]]
splitTwos (x:y:xs) = [x,y] : splitTwos xs
splitTwos (x:[]) = [[x]]

makePairs :: Int -> [Int]
makePairs n
	| odd $ length s = map read $ splitTwos $ ('0':s) ++ (repeat '0')
	| otherwise = map read $ splitTwos $ s ++ (repeat '0')
	where s = show n

digitSum :: [Char] -> Int
digitSum s = sum $ map digitToInt s

numDigits n d = head $ dropWhile ((<d) . length) $ map (\x -> show $ isqrt (n*10^(2*x))) [0..]

nonSquares = [1..100] \\ (takeWhile (<=100) $ map (^2) [1..])

main = print $ sum $ map (\x -> digitSum $ numDigits x 100) nonSquares