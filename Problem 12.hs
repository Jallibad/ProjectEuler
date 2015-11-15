import MathFunctions (factors)
import FigurateNumbers (triangles)

main = print $ fst $ head $ dropWhile ((<500) . snd) $ zip triangles $ map (length . factors) triangles