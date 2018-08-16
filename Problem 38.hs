import Data.List (sort)

concatProduct :: Integral (a) => Show (a) => a -> a -> String
concatProduct n test = concatMap (show . (test*)) [1..n]

valid :: String -> Bool
valid n = sort n == ['1'..'9']

main = print $ maximum [y | x <- [2..9], y <- takeWhile ((<10) . length) $ dropWhile ((<9) . length) $ map (concatProduct x) [1..], valid y]