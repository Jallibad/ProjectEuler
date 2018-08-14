{-# LANGUAGE ViewPatterns #-}

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Arrow
import Data.Maybe
import Data.List
import Data.List.Split
import Data.List.Ordered (has)
import Control.Parallel.Strategies

replaceWords :: String -> [Int]
replaceWords word = map (\rep -> read $ map (rep Map.!) word) $ replaceLetters $ replacements word

replaceLetters :: Map.Map Char [Char] -> [Map.Map Char Char]
replaceLetters thing = case (Map.minViewWithKey thing) of
	Nothing -> return Map.empty
	Just ((x,y), m) -> concatMap (\b -> map (Map.insert x b) $ replaceLetters $ fmap (delete b) m) y

replacements :: String -> Map.Map Char [Char]
replacements word = Map.fromSet (letterFunction word) $ Set.fromList word

letterFunction :: String -> Char -> [Char]
letterFunction word l
	| l == (head word) = ['1'..'9']
	| l == (last word) = ['0','1','4','5','6','9']
	| otherwise = ['0'..'9']

--replaceGen :: Map.Map Char Int -> String -> String -> (Map.Map Char Int, String)

isSquare :: Int -> Bool
isSquare = has squares
	where squares = map (^2) [1..]

readWords :: String -> [(String, [String])]
readWords = map (sort &&& pure) . splitOn "," . filter (/='"')

main = readFile "Problem 98 Words.txt" >>= print . Map.filter ((>1) . length) . Map.fromListWith (++) . readWords