import Data.Char

factorial :: Integral (a) => a -> a
factorial number = product [1..number]

main = print $ sum $ map digitToInt $ show $ factorial 100