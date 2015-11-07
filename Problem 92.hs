import Data.Char

factorial :: Integral (a) => a -> a
factorial number = product [1..number]

digitFactorial :: Integer -> Integer
digitFactorial number = toInteger $ sum $ map (\x -> (digitToInt x)^2) $ show number

main = print $ length $ filter (==89) $ map (head . dropWhile (\x -> (x/=1)&&(x/=89)) . iterate digitFactorial) [1..10^7]