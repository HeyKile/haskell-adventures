-- Kyle Bello
-- Functional Programming
-- Assignment 5: Cash Register

import System.IO ( hFlush, stdout )
import Data.Char ()
import Receipt (Receipt, constructReciept, addReceiptItem, printReceipt, getTotalPrice, finalizeReceipt)
import Text.Printf (printf)
import Text.Read (readMaybe)
import Config (toChange)

main :: IO ()
main = do
    (first, last, email) <- initUser
    mainMenu first last email

-- initalizes the current user
initUser :: IO (String, String, String)
initUser = do
    firstName <- prompt "Enter your first name: "
    lastName <- prompt "Enter your last name: "
    email <- prompt "Enter your email: "
    putStrLn "================================"
    return (firstName, lastName, email)

-- displays main menu of the program
mainMenu :: String -> String -> String -> IO ()
mainMenu first last email = do
    displayMenuOptions
    selection <- prompt "What would you like to do? "
    if selection == "1"
        then do
            runTransaction (constructReciept first last email [])
            mainMenu first last email
    else if selection == "2"
        then return ()
    else
        mainMenu first last email

-- displays main menu options
displayMenuOptions :: IO ()
displayMenuOptions = do
    putStrLn "Main Menu"
    putStrLn "========="
    putStrLn "1. Start a new transaction"
    putStrLn "2. Exit"

-- transaction runner
runTransaction :: Receipt -> IO ()
runTransaction receipt = do   
    let curPrinted = printReceipt receipt
    displayReceipt curPrinted
    putStrLn "(1) Add new item (2) End transaction (3) Cancel transaction"
    selection <- prompt "What would you like to do? "
    if selection == "1" -- add new item
        then do
            itemName <- prompt "Enter item name: "
            priceStr <- prompt "Enter price: "
            let priceMaybe = readMaybe priceStr :: Maybe Float
            case priceMaybe of
                Just price -> do
                    let newReceipt = addReceiptItem receipt itemName price
                    runTransaction newReceipt
                Nothing -> do
                    putStrLn "Invalid price entered!"
                    runTransaction receipt
    else if selection == "2" -- end transaction 
        then endTransaction receipt
    else if selection == "3" -- cancel transaction
        then putStrLn "Transaction cancelled"
    else -- else, continue
        runTransaction receipt

-- ends a transaction for a given receipt
endTransaction :: Receipt -> IO ()
endTransaction receipt = do
    paymentStr <- prompt "Enter payment amount: "
    let paymentMaybe = readMaybe paymentStr :: Maybe Float
    case paymentMaybe of
        Just payment -> do
            let total = getTotalPrice receipt
            if payment < total
                then do
                    putStrLn "Not enough payment! "
                    endTransaction receipt
            else do
                let change = payment - total
                let changeStr = "Change: " ++ printf ("%." ++ show 2 ++ "f") change
                displayReceipt [changeStr, toChange change]
                let finalRec = finalizeReceipt receipt payment total
                displayReceipt finalRec

        Nothing -> do
            putStrLn "Invalid payment entered!"
            endTransaction receipt

-- prints the lines of a given receipt
displayReceipt :: [String] -> IO ()
displayReceipt [] = return ()
displayReceipt (x:xs) = do
    putStrLn x
    displayReceipt xs

-- helper function I stole from the slides
prompt :: String -> IO String  
prompt str = do
    putStr str
    hFlush stdout
    getLine