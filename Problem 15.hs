factorial :: Integral (a) => a -> a
factorial 0 = 1
factorial n = product [1..n]

combinatoric :: Integral (a) => a -> a -> a
combinatoric n r = (factorial n) `div` ((factorial (n-r))*(factorial r))

main = print $ combinatoric (20+20) 20