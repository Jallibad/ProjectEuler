fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

main = print $ until ((==) 1000 . length . show . (!!) fibs) (+1) 1