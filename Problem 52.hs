import ListFunctions (allEqual)
import Data.List (sort)

main = print $ until (\x -> allEqual $ map (sort . show . (x*)) [1..6]) (+1) 1