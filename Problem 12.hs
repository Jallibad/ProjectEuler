import MathFunctions (factors)
import FigurateNumbers (triangles)

main = print $ head $ dropWhile ((<500) . length . factors) triangles