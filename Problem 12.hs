import FigurateNumbers (triangles)
import MathFunctions (factors)

main = print $ head $ dropWhile ((<500) . length . factors) triangles