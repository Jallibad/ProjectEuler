import Fraction

main = print $ numerator $ until ((>=10^6) . denominator) (mediant $ Fraction 3 7) 0
