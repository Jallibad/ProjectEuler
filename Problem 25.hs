import MathFunctions (fibs)

main = print $ fst $ head $ until ((==1000) . length . show . snd . head) tail $ zip [0..] fibs