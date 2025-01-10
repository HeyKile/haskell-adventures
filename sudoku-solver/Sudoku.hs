-- Module for suodoku type
module Sudoku (Sudoku(..), solve) where

import Data.List (find)

newtype Sudoku = Sudoku String

-- pretty prints a sudoku puzzel
instance Show Sudoku where
    show :: Sudoku -> String
    show x = showSudoku x 0

-- helper printer function
showSudoku :: Sudoku -> Int -> String
showSudoku (Sudoku []) _ = printBoarder
showSudoku (Sudoku xs) 0 = printBoarder ++ printRow (take 9 xs) ++ showSudoku (Sudoku (drop 9 xs)) 1
showSudoku (Sudoku xs) count
    | count `mod` 3 == 0 = printBoarder ++ printRow (take 9 xs) ++ showSudoku (Sudoku (drop 9 xs)) (count + 1)
    | otherwise          = printSeperator ++ printRow (take 9 xs) ++ showSudoku (Sudoku (drop 9 xs)) (count + 1)

-- prints the current row of of numbers
printRow :: String -> String
printRow [] = "|\n"
printRow (x:y:z:zs) = printTriple x y z ++ printRow zs

-- formats and prints current row's triple
printTriple :: Char -> Char -> Char -> String
printTriple x y z = "|" ++ printNum x ++ ":" ++ printNum y ++ ":" ++ printNum z

-- formats and prints board num/space
printNum :: Char -> String
printNum x = " " ++ [x] ++ " "

-- prints a grid boarder
printBoarder :: String
printBoarder = concat (replicate 9 "+===") ++ "+\n"

-- prints a grid seperator
printSeperator :: String
printSeperator = concat (replicate 9 "+---") ++ "+\n"

-- solver function
solve :: Sudoku -> Maybe Sudoku
solve (Sudoku []) = Nothing
solve (Sudoku xs) = case solveSudoku (formatSolverStr xs) 0 0 of
    Nothing -> Nothing
    Just board -> Just board

-- solves the sudoku
solveSudoku :: [String] -> Int -> Int -> Maybe Sudoku
solveSudoku board row col
    | row > 8 = Just (Sudoku (concat board))
    | board !! row !! col /= ' ' = solveSudoku board (nextRow row col) (nextCol col)
    | otherwise = tryValidNums board row col (findValidNums board row col)

-- calculates next row
nextRow :: (Eq a1, Num a1, Num a2) => a2 -> a1 -> a2
nextRow row col = if col == 8 then row + 1 else row

-- calculates next col
nextCol :: (Eq a, Num a) => a -> a
nextCol col = if col == 8 then 0 else col + 1

-- finds all numbers that could be valud
findValidNums :: [String] -> Int -> Int -> [Char]
findValidNums board row col = filter (isValid board row col) ['1'..'9']

-- trys the current valid nums
tryValidNums :: [String] -> Int -> Int -> [Char] -> Maybe Sudoku
tryValidNums _ _ _ [] = Nothing
tryValidNums board row col (n:ns) = case solveSudoku (placeBoardNum board row col n) (nextRow row col) (nextCol col) of
    Nothing -> tryValidNums board row col ns
    solution -> solution

-- places the current char onto the board 
placeBoardNum :: [String] -> Int -> Int -> Char -> [String]
placeBoardNum board row col num = take row board ++ [take col (board !! row) ++ [num] ++ drop (col + 1) (board !! row)] ++ drop (row + 1) board

-- checks if current num is in the row, col, or box
isValid :: [String] -> Int -> Int -> Char -> Bool
isValid board row col num = num `notElem` (getRow board row ++ getCol board col ++ getBox board row col)

-- gets current row
getRow :: [String] -> Int -> String
getRow board row = board !! row

-- gets current col
getCol :: [String] -> Int -> String
getCol board col = map (!! col) board

-- gets current box
getBox :: [String] -> Int -> Int -> String
getBox board row col = concat [take 3 (drop (col - mod col 3) (board !! r)) | r <- [boxStartRow..boxStartRow+2]]
    where
        boxStartRow = row - mod row 3

-- turns 1D board rep into 2D
formatSolverStr :: String -> [String]
formatSolverStr [] = []
formatSolverStr xs = take 9 xs : formatSolverStr (drop 9 xs)