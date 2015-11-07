import Data.List
import Control.Applicative

isqrt :: Integral (a) => a -> a
isqrt 0 = 0
isqrt 1 = 1
isqrt n = head $ dropWhile (\x -> x*x > n) $ iterate (\x -> (x + n `div` x) `div` 2) (n `div` 2)

isPrime :: Integral (a) => a -> Bool
isPrime 1 = False
isPrime number = all ((/=) 0 . mod number) [2..isqrt number]

isTruncatablePrime :: Show (a) => Integral (a) => a -> Bool
isTruncatablePrime n = (n >= 10) && (all (isPrime . read) $ (tail $ inits s)++(init $ tails s))
	where s = show n

possibilities :: [Int]
possibilities = map read $ concat $ iterate (\x -> (++) <$> x <*> singlePrimes) singlePrimes
	where singlePrimes = ["1","2","3","5","7","9"]

main = print $ sum $ take 11 $ filter isTruncatablePrime possibilities