{-# LANGUAGE TupleSections, TemplateHaskell, ParallelListComp, Rank2Types, ViewPatterns #-}

import Control.Applicative
import Control.Lens
import Control.Lens.Setter
import Data.Array
import Data.Bifunctor
import Data.Char
import Data.Either
import Data.Foldable (foldr', find)
import Data.List (sortBy, delete)
import Data.List.Split
import Data.Maybe
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import qualified Data.Set as Set
import Debug.Trace

data GridSquare = Completed {_completed :: Int} | Uncompleted {_uncompleted :: Set.Set Int}

makeLenses ''GridSquare

--instance Bifunctor GridSquare 

constructGridSquare :: Set.Set Int -> GridSquare
constructGridSquare set
	| Set.size set == 1 = Completed $ Set.findMax set
	| otherwise = Uncompleted set

peFormatToGridSquare :: Char -> GridSquare
peFormatToGridSquare '0' = constructGridSquare fullyUnknown
peFormatToGridSquare x = Completed $ digitToInt x

removeOption :: GridSquare -> Int -> GridSquare
removeOption (Uncompleted set) n = constructGridSquare $ Set.delete n set
removeOption x _ = x

options :: GridSquare -> Int
options square = Set.size $ square^.uncompleted

instance Show GridSquare where
	show (Completed g) = show g
	show _ = "0"

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
readGrids (splitAt 10 -> (title:x,xs)) = Map.insert title (readGrid $ concat x) $ readGrids xs

grid1 = readGrid "003020600900305001001806400008102900700000008006708200002609500800203009005010300"
grid2 = readGrid "200080300060070084030500209000105408000000000402706000301007040720040060004010003"
grid6 = readGrid "100920000524010000000000070050008102000000000402700090060000000000030945000071006"

removeLinesAndGrids :: Sudoku -> Sudoku
removeLinesAndGrids (Sudoku grid) = Sudoku $ accum removeOption grid $ concat rowsToUpdate
	where rowsToUpdate = [(fillRow a)++(fillColumn a)++(fillGrid a) | ((x,y), Completed e) <- assocs grid, let a=((x,y),e)]

toMap :: GridSquare -> Map.Map Int (Maybe Int)
toMap (Completed x) = Map.adjust (const Nothing) x $ Map.fromAscList $ zip [1..9] $ repeat $ Just 0
toMap (Uncompleted set) = Map.fromSet (const $ Just 1) set

fix :: Sudoku -> Lens' Sudoku [GridSquare] -> Sudoku
fix grid lens = over lens func grid
	where
		func squares = map (setIf $ findLoners squares) squares
		setIf x s = fromMaybe s $ fmap Completed $ find (const True) $ Set.intersection x $ s^.uncompleted
		findLoners = Map.keysSet . Map.filter (== Just 1) . Map.unionsWith (liftA2 (+)) . map toMap

completeLoneRows grid = foldl (\grid x -> fix grid $ rowLens x) grid [0..8]
completeLoneColumns grid = foldl (\grid y -> fix grid $ columnLens y) grid [0..8]
completeLoneGrids grid = foldl (\grid p -> fix grid $ gridLens p) grid [(x,y) | x <- [0,3..8], y <- [0,3..8]]

singleLens i = lens getter setter
	where
		getter (Sudoku grid) = grid ! i
		setter (Sudoku grid) v = Sudoku $ grid // [(i, v)]

rowLens :: Int -> Lens' Sudoku [GridSquare]
rowLens y = lens getter setter
	where
		getter (Sudoku grid) = elems $ ixmap ((0,y),(8,y)) id $ grid
		setter (Sudoku grid) vs = Sudoku $ grid // [((x,y),v) | x <- [0..8] | v <- vs]

columnLens x = lens getter setter
	where
		getter (Sudoku grid) = elems $ ixmap ((x,0),(x,8)) id $ grid
		setter (Sudoku grid) vs = Sudoku $ grid // [((x,y),v) | y <- [0..8] | v <- vs]

gridLens (x,y) = lens getter setter
		where
			lowX = x-(x `mod` 3)
			lowY = y-(y `mod` 3)
			getter (Sudoku grid) = elems $ ixmap ((lowX,lowY),(lowX+2,lowY+2)) id $ grid
			setter (Sudoku grid) vs = Sudoku $ grid // [((x,y),v) | x <- [lowX..lowX+2], y <- [lowY..lowY+2] | v <- vs]

fillRow ((x,y),a) = map ((,a) . (,y)) [0..8]
fillColumn ((x,y),a) = map ((,a) . (x,)) [0..8]
fillGrid ((x,y),a) = [((xs,ys),a) | xs <- [lowX.. lowX+2], ys <- [lowY.. lowY+2], xs/=x || ys/=y]
	where
		lowX = (x `div` 3)*3
		lowY = (y `div` 3)*3

solvePuzzle :: Sudoku -> Sudoku
solvePuzzle grid = head $ dropWhile ((/=0) . remainingUnknowns) $ scanl (flip ($)) grid inferenceRules
	where
		inferenceRules = cycle [removeLinesAndGrids, completeLoneRows, completeLoneColumns, completeLoneGrids]

workOnPuzzle grid = scanl (flip ($)) grid inferenceRules
	where
		inferenceRules = cycle [removeLinesAndGrids, completeLoneRows, completeLoneColumns, completeLoneGrids]

main = readFile "Problem 96 sudoku.txt" >>= print . fmap solvePuzzle . readGrids . lines

setPicker :: Int -> [Set.Set Int] -> Set.Set Int -> [Set.Set Int]
setPicker num [] ans
	| num == Set.size ans = [ans]
	| otherwise = []
setPicker num sets ans = concatMap f sets

f set = setPicker (num+(Set.size set)-1) (delete set sets) (Set.union set ans)
	where
		nSet = delete set sets