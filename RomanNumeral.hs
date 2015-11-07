module RomanNumeral where

import Data.List
import Data.Maybe
import Data.Function
import Control.Applicative

data RomanNumeral = RomanNumeral String

instance Show RomanNumeral where
	show (RomanNumeral s) = s

toRomanNumeral :: Int -> RomanNumeral
toRomanNumeral = RomanNumeral . toRomanString

numerals = [('M',1000),('D',500),('C',100),('L',50),('X',10),('V',5),('I',1)]

negates =
	[
		(4, 5, "IV"),
		(9, 10, "IX"),
		(40, 50, "XL"),
		(90, 100, "XC"),
		(400, 500, "CD"),
		(900, 1000, "CM")
	]

toRomanString :: Int -> String
toRomanString 0 = ""
toRomanString number = case negateElem of
				Just (min, max, numeral) -> numeral ++ (toRomanString $ number-min)
				Nothing -> nextNumeral : (toRomanString (number-nextValue))
					where (nextNumeral, nextValue) = head $ dropWhile ((> number) . snd) numerals
	where	negateElem = (negates !!) <$> ((\(min, max, _) -> min <= number && number < max) `findIndex` negates)

fromRomanNumeral :: RomanNumeral -> Int
fromRomanNumeral = fromRomanString . show

fromRomanString :: String -> Int
fromRomanString [] = 0 
fromRomanString xs = case negateElem of
			Just (min, _, _) -> min + (fromRomanString $ drop 2 xs)
			Nothing -> (fromJust $ lookup (head xs) numerals) + (fromRomanString $ tail xs)
	where negateElem = find (\(_, _, x) -> x == (take 2 xs)) negates

simplify :: RomanNumeral -> RomanNumeral
simplify = toRomanNumeral . fromRomanNumeral

instance Eq RomanNumeral where
	(==) = (==) `on` fromRomanNumeral