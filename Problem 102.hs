data Point = Point {x :: Int, y :: Int} deriving (Show)
data Triangle = Triangle {a :: Point, b :: Point, c :: Point} deriving (Show)

containsOrigin :: Triangle -> Bool
containsOrigin (Triangle (Point x1 y1) (Point x2 y2) (Point x3 y3)) = (0 <= t1) && (t1 <= 1) && (0 <= t2) && (t2 <= 1) && (s <= 1)
	where
		denominator = (y2-y3)*(x1-x3)+(x3-x2)*(y1-y3)
		t1 = (fromIntegral $ (y2-y3)*(-x3)+(x3-x2)*(-y3)) / (fromIntegral denominator)
		t2 = (fromIntegral $ (y3-y1)*(-x3)+(x1-x3)*(-y3)) / (fromIntegral denominator)
		s = t1+t2

readTriangle :: [Point] -> Triangle
readTriangle (a:b:c:[]) = Triangle a b c
readTriangle _ = error "wrong number of points"

readPoints :: [Int] -> [Point]
readPoints [] = []
readPoints ints = (Point x y) : readPoints xs
	where (x:y:[], xs) = splitAt 2 ints

readInts :: String -> [Int]
readInts [] = []
readInts string = (read f) : (readInts $ drop 1 s)
	where (f, s) = break (==',') string

main = do
	file <- readFile "Problem 102 Triangles.txt"
	print $ length $ filter (containsOrigin . readTriangle . readPoints . readInts) $ lines file