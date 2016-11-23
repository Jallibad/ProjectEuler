import Fraction

rounds = map (\x -> [(1, 0, Fraction 1 x), (0, 1, Fraction (x-1) x)]) [2..16] -- [2..n+1] to get n rounds

winProbability = sum $ map (\(_, _, x) -> x) $ filter (\(a, b, _) -> a > b) $ foldl1 (\f1 f2 -> concatMap (\(a1, b1, c1) -> map (\(a2, b2, c2) -> (a1+a2, b1+b2, c1*c2)) f2) f1) rounds

main = print $ truncate $ value $ Fraction (denominator winProbability) (numerator winProbability)