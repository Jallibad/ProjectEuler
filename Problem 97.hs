takeLast :: Int -> [a] -> [a]
takeLast number list = drop (length list - number) list

main = print $ takeLast 10 $ show (28433*2^7830457+1)