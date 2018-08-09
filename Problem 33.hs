import Fraction

main = print $ denominator $ product [Fraction n d | i <- [1..9], d <- [1..9], n <- [1..d-1], i/=n, i/=d, (d*(10*n+i))==(n*(10*i+d))]