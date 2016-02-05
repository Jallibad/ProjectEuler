import Data.Function
import Fraction

main = print $ numerator $ last $ takeWhile ((<=10^6) . denominator) $ iterate (simplify . (mediant $ Fraction 3 7)) $ Fraction 0 1