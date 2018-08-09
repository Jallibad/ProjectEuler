import qualified Data.Set as Set
import MathFunctions

upperBound = 5*10^7

squares = takeWhile (<upperBound) $ map (^2) primes
cubes = takeWhile (<upperBound) $ map (^3) primes
tesseracts = takeWhile (<upperBound) $ map (^4) primes

main = print $ Set.size $ Set.fromList [tesseract+cube+square | tesseract <- tesseracts, cube <- takeWhile (<(upperBound-tesseract)) cubes, square <- takeWhile (<(upperBound-tesseract-cube)) squares]