import Data.List

allTheSame :: Eq (a) => [a] -> Bool
allTheSame [] = True
allTheSame (x:xs) = all (==x) xs

main = print $ until (\x -> allTheSame $ map (\y -> sort $ show $ y*x) [1..6]) (+1) 1