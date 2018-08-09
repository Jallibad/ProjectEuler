import ListFunctions (infIntersect)

--Hexagonal numbers are also triangle numbers
pentagonals = map (\n -> n*(3*n-1) `div` 2) [1..]
hexagonals = map (\n -> n*(2*n-1)) [1..]

main = print $ (infIntersect hexagonals pentagonals) !! (3-1)