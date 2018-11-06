{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

import Control.Monad (void)
import Text.Parsec (Stream, ParsecT, (<|>), char, digit, endOfLine, many, many1, parse)

data Point = Point {x :: Int, y :: Int} deriving (Show)
data Triangle = Triangle {a :: Point, b :: Point, c :: Point} deriving (Show)

type Parser a = forall s u m. Stream s m Char => ParsecT s u m a

unsignedInteger :: (Integral a, Read a) => Parser a
unsignedInteger = read <$> many1 digit

integer :: (Integral a, Read a) => Parser a
integer = (char '-' >> negate <$> unsignedInteger) <|> unsignedInteger

comma :: Parser ()
comma = void $ char ','

point :: Parser Point
point = Point <$> integer <* comma <*> integer

triangle :: Parser Triangle
triangle = Triangle <$> point <* comma <*> point  <* comma <*> point

triangles :: Parser [Triangle]
triangles = many $ triangle <* endOfLine

containsOrigin :: Triangle -> Bool
containsOrigin (Triangle (Point x1 y1) (Point x2 y2) (Point x3 y3)) = between 0 1 t1 && between 0 1 t2 && s <= 1
	where
		denominator = (y2-y3)*(x1-x3)+(x3-x2)*(y1-y3)
		t1 = (fromIntegral $ (y2-y3)*(-x3)+(x3-x2)*(-y3)) / (fromIntegral denominator)
		t2 = (fromIntegral $ (y3-y1)*(-x3)+(x1-x3)*(-y3)) / (fromIntegral denominator)
		between l u n = l <= n && n <= u
		s = t1+t2

main = do
	file <- readFile "Problem 102 Triangles.txt"
	let numContainingOrigin = length . filter containsOrigin
	print $ numContainingOrigin <$> parse triangles "" file