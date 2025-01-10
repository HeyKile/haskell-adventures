-- Kyle Bello
-- item data type for use in CashRegister.hs
module Item (Item, createItem, getItemStrLength, getItemPrice, printItems, payTotal) where

import Text.Printf (printf)

-- Item data type
data Item = Item { itemName :: String, price :: Float } deriving (Show)

-- ================== Module Functions ==================

-- creates an item from a given name and price
createItem :: String -> Float -> Item
createItem = Item

-- gets the length of a given item's string rep
getItemStrLength :: Item -> Int
getItemStrLength item = length (showItemName item ++ showItemPrice item)

-- returns the price of an item as a float
getItemPrice :: Item -> Float
getItemPrice (Item _ price) = price

-- formats and prints a given list of items based on a given spacing
printItems :: [Item] -> Int -> [String]
printItems items spacing = [addPrintSpacing "| Description" "Total |" spacing] ++ genPrintableItems items spacing ++ [addPrintSpacing "|" "|" spacing] ++ [genTotal items spacing]

-- generates strings for paying a transaction
payTotal :: Float -> Float -> Int -> [String]
payTotal payment total lineLen = addPrintSpacing "| Cash" (formatPrice payment 2 ++ " |") lineLen : [addPrintSpacing "| Change" (formatPrice change 2 ++ " |") lineLen]
    where change = payment - total

-- ================== Module Functions ==================

-- returns str rep of an item's name
showItemName :: Item -> String
showItemName (Item itemName _) = itemName

-- returns str rep of an item's price
showItemPrice :: Item -> String
showItemPrice (Item _ price) = formatPrice price 2

-- formats a float str to 2 decimal places
formatPrice :: Float -> Int -> String
formatPrice fl n = printf ("%." ++ show n ++ "f") fl

-- generates list of formatted strings based on a given item list
genPrintableItems :: [Item] -> Int -> [String]
genPrintableItems items spacing
    | null items  = [addPrintSpacing "|" "|" spacing]
    | spacing < 0 = error "Spacing less than 0"
    | otherwise   = [genPrintableItem str spacing | str <- items]

-- generates a receipt string from a given item and line spacing
genPrintableItem :: Item -> Int -> String
genPrintableItem item = addPrintSpacing ("| " ++ showItemName item) (showItemPrice item ++ " |")

-- generates receipt string for the current transaction total
genTotal :: [Item] -> Int -> String
genTotal items = addPrintSpacing "| Total" (formatPrice total 2 ++ " |")
    where total = sum [getItemPrice item | item <- items]

-- adds spacing to a given 
addPrintSpacing :: String -> String -> Int -> String
addPrintSpacing itemName priceStr spacing = itemName ++ replicate replicateAmt ' ' ++ priceStr
    where replicateAmt = spacing - length (itemName ++ priceStr)