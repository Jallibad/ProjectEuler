{-# LANGUAGE TupleSections #-}

import Data.Array
import Data.Bifunctor
import Data.Char
import Data.Either
import Data.Foldable (foldr')
import Data.List.Split
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

newtype GridSquare = GridSquare (Either Int (Set.Set Int))

constructGridSquare :: Either Int (Set.Set Int) -> GridSquare
constructGridSquare (Right set)
	| Set.size set == 1 = GridSquare $ Left $ Set.findMax set
	| otherwise = GridSquare $ Right set
constructGridSquare x = GridSquare x

peFormatToGridSquare :: Char -> GridSquare
peFormatToGridSquare '0' = constructGridSquare $ Right fullyUnknown
peFormatToGridSquare x = constructGridSquare $ Left $ digitToInt x

isCompleted :: GridSquare -> Bool
isCompleted (GridSquare x) = isLeft x

removeOption :: GridSquare -> Int -> GridSquare
removeOption (GridSquare x) n = constructGridSquare $ (second . Set.delete) n x

options :: GridSquare -> Int
options (GridSquare (Left _)) = 0
options (GridSquare (Right set)) = Set.size set - 1

instance Show GridSquare where
	show (GridSquare g) = show $ either id (const 0) g

newtype Sudoku = Sudoku (Array (Int, Int) GridSquare)

instance Show Sudoku where
	show (Sudoku grid) = unlines $ chunksOf 9 $ map (head . show) $ elems grid

remainingUnknowns :: Sudoku -> Int
remainingUnknowns (Sudoku grid) = foldr' ((+) . options) 0 grid

fullyUnknown = Set.fromDistinctAscList [1..9]

readGrid :: String -> Sudoku
readGrid = Sudoku . fmap peFormatToGridSquare . listArray ((0, 0), (8, 8))

readGrids :: [String] -> Map.Map String Sudoku
readGrids [] = Map.empty
readGrids s = Map.insert title (readGrid $ concat x) $ readGrids xs
	where (title:x,xs) = splitAt 10 s

grid1 = readGrid "003020600900305001001806400008102900700000008006708200002609500800203009005010300"
grid2 = readGrid "200080300060070084030500209000105408000000000402706000301007040720040060004010003"

removeLinesAndGrids :: Sudoku -> Sudoku
removeLinesAndGrids (Sudoku grid) = Sudoku $ accum removeOption grid $ concat rowsToUpdate
	where rowsToUpdate = [(fillRow a)++(fillColumn a)++(fillGrid a) | ((x,y), GridSquare (Left e)) <- assocs grid, let a=((x,y),e)]

fillRow ((x,y),a) = map ((,a) . (,y)) [0..8]
fillColumn ((x,y),a) = map ((,a) . (x,)) [0..8]
fillGrid ((x,y),a) = [((xs,ys),a) | xs <- [lowX.. lowX+2], ys <- [lowY.. lowY+2], xs/=x || ys/=y]
	where	lowX = (x `div` 3)*3
		lowY = (y `div` 3)*3

solvePuzzle :: Sudoku -> Sudoku
solvePuzzle = head . dropWhile ((/=0) . remainingUnknowns) . iterate removeLinesAndGrids

main = readFile "Problem 96 sudoku.txt" >>= print . fmap solvePuzzle . readGrids . lines