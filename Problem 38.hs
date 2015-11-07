import Data.List

concatProduct :: Integral (a) => Show (a) => a -> a -> String
concatProduct test n = concatMap (show . (*test)) [1..n]

valid :: String -> Bool
valid n = (null $ ['1'..'9'] \\ n) && (length n == 9)



main = print $ maximum $ filter valid (concatMap (\y -> takeWhile ((>) 10 . length) $ dropWhile ((>) 9 . length) $ map (\x -> concatProduct x y) [1..]) [2..9])