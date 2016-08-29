import Data.List (sort)

concatProduct :: Integral (a) => Show (a) => a -> a -> String
concatProduct n test = concatMap (show . (test*)) [1..n]

valid :: String -> Bool
valid n = sort n == ['1'..'9']

main = print $ maximum $ filter valid (concatMap (\y -> takeWhile ((>) 10 . length) $ dropWhile ((>) 9 . length) $ map (concatProduct y) [1..]) [2..9])