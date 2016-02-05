module FigurateNumbers where

triangleNumber :: Integral (a) => a -> a
triangleNumber index = sum [1..index]

triangles = map (\n -> n*(n+1) `div` 2) [1..]
squares = map (^2) [1..]
pentagonals = map (\n -> n*(3*n-1) `div` 2) [1..]
hexagonals = map (\n -> n*(2*n-1)) [1..]
heptagonals = map (\n -> n*(5*n-3) `div` 2) [1..]
octagonals = map (\n -> n*(3*n-2)) [1..]