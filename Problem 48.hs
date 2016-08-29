import ListFunctions (takeLast)

main = print $ takeLast 10 $ show $ sum [x^x | x <- [1..1000]]