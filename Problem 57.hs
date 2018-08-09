import Data.Function (on)

pellNumbers :: [Integer]
pellNumbers = 2 : 5 : zipWith (\x y -> x+2*y) pellNumbers (tail pellNumbers)

pellNumerators :: [Integer]
pellNumerators = 3 : zipWith (+) pellNumbers (tail pellNumbers)

main = print $ length $ filter (uncurry ((>) `on` (length . show))) $ take 1000 $ zip pellNumerators pellNumbers