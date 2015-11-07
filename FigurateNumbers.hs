module FigurateNumbers where

triangleNumber :: Integral (a) => a -> a
triangleNumber index = sum [1..index]