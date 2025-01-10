-- Kyle Bello - Sudoku Solver
import Sudoku
import Text.Printf (printf)
import System.IO (hFlush, stdout)

main :: IO ()
main = do
    str <- getPuzzleStr "" 0
    -- let str = "53  7    6  195    98    6 8   6   34  8 3  17   2   6 6    28    419  5    8  79"
    let sudoku = Sudoku str
    putStrLn (show sudoku)
    putStrLn "Solving..."
    case solve sudoku of
        Nothing -> putStrLn "Puzzle is unsolveable!"
        Just solution -> putStrLn (show solution)

-- gets the user input for the board
-- Note: does not autofill spaces
getPuzzleStr :: String -> Int -> IO String
getPuzzleStr str count
    | count == 0 = do
        nextLine <- prompt "Enter a Sudoku puzzle row-by-row (press enter after each row):\n"
        if length nextLine == 9
            then getPuzzleStr (str ++ nextLine) (count + 1)
            else do
                putStrLn "Entry isn't 9 chars, please renter:"
                getPuzzleStr str count
    | count == 9 = do
        return str
    | otherwise  = do
        nextLine <- prompt ""
        if length nextLine == 9
            then getPuzzleStr (str ++ nextLine) (count + 1)
            else do
                putStrLn "Entry isn't 9 chars, please renter:"
                getPuzzleStr str count


-- helper function I stole from the slides
prompt :: String -> IO String
prompt str = do
    putStr str
    hFlush stdout
    getLine