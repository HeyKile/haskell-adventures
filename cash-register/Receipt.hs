-- Kyle Bello
-- data types for use in CashRegister.hs

module Receipt (Receipt, constructReciept, addReceiptItem, printReceipt, getTotalPrice, finalizeReceipt) where

import Item (createItem, printItems, getItemStrLength, getItemPrice, payTotal, Item)
import Config (baseReceiptLineOffset, baseReceiptSize)

-- ================== Data Types ==================

-- receipt data type
data Receipt = Receipt { firstName :: String, lastName :: String, email :: String, items :: [Item] }

-- ================== Module Functions ==================

-- constructs a recipet based on a given first, last name, and email
constructReciept :: String -> String -> String -> [Item] -> Receipt
constructReciept = Receipt

-- creates and item based off a given name and price and adds it to a given receipt
addReceiptItem :: Receipt -> String -> Float -> Receipt
addReceiptItem (Receipt f l e items) newItemName newItemPrice = Receipt f l e (items ++ [createItem newItemName newItemPrice])

-- prints a given receipt in proper formatting
printReceipt :: Receipt -> [String]
printReceipt (Receipt first last email items) = genReceiptHeaders first last email lineLen ++ printItems items lineLen ++ [printBottom lineLen]
    where lineLen = getLineLength (Receipt first last email items)

-- returns the total transaction price for a given receipt
getTotalPrice :: Receipt -> Float
getTotalPrice (Receipt _ _ _ items) = sum [getItemPrice item | item <- items]

-- finalizes and formats a given receipt based of a given payment
finalizeReceipt :: Receipt -> Float -> Float -> [String]
finalizeReceipt (Receipt first last email items) payment total = genReceiptHeaders first last email lineLen ++ 
                                                    printItems items lineLen ++ payTotal payment total lineLen ++ [printBottom lineLen]
    where lineLen = getLineLength (Receipt first last email items)

-- ================== Helper Functions ==================

-- gets the current line length for a given receipt
getLineLength :: Receipt -> Int
getLineLength receipt = max (longestReceiptLine receipt + baseReceiptLineOffset) baseReceiptSize

-- Finds the current longest item from a given receipt
longestReceiptLine :: Receipt -> Int
longestReceiptLine (Receipt f l e items) = maximum ([getItemStrLength item | item <- items] ++ [length f] ++ [length l] ++ [length e])

-- generates receipt header for a given spacing
genReceiptHeaders :: String -> String -> String -> Int -> [String]
genReceiptHeaders first last email spacing =
    [addHeaderSpacing "" spacing '+' '-'] ++ [addHeaderSpacing first spacing '|' ' '] ++
    [addHeaderSpacing last spacing '|' ' '] ++ [addHeaderSpacing email spacing '|' ' '] ++
    [addHeaderSpacing "" spacing '|' ' ']

-- generates a header string based on the giving spacing
-- lineEnds denotes how the header line with start/end ('+' or '|')
-- spacer is what the spacing char is ('-' or ' ')
addHeaderSpacing :: String -> Int -> Char -> Char -> String
addHeaderSpacing str spacing lineEnds spacer = beg ++ replicate half1 spacer ++ str ++ replicate half2 spacer ++ end
    where
        beg = lineEnds : [spacer]
        end = spacer : [lineEnds]
        spacingToAdd = spacing - length str - length beg * 2
        half1 = spacingToAdd `div` 2
        half2 = spacingToAdd - half1

-- prints the bottom of the receipt
printBottom :: Int -> String
printBottom lineLen = addHeaderSpacing "" lineLen '+' '-'
