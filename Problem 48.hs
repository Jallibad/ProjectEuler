takeLast :: Int -> [a] -> [a]
takeLast number list = drop (length list - number) list

main = print $ takeLast 10 $ show $ sum [x^x | x <- [1..1000]]