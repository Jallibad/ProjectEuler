import MathFunctions (distinctPrimeFactors)

distinctPrimeFactorsGenerator :: Int -> Int
distinctPrimeFactorsGenerator d = fst $ head $ filter (all (==d) . take d . snd) $ zip [1..] $ iterate tail $ map (length . distinctPrimeFactors) [1..]

main = print $ distinctPrimeFactorsGenerator 4