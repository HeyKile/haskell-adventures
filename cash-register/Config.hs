-- Some helper functions for the cash register program

module Config (baseReceiptLineOffset, baseReceiptSize, toChange) where

baseReceiptSize :: Int
baseReceiptSize = 32

baseReceiptLineOffset :: Int
baseReceiptLineOffset = 5

toChange :: Float -> String
toChange n = 
    let (dollars, cents)   = properFraction n
        (hundreds, d100)   = dollars `divMod` 100
        (twentys, d20)     = d100 `divMod` 20
        (tens, d10)        = d20 `divMod` 10
        (fives, d5)        = d10 `divMod` 5
        centsInt           = round (cents * 100)
        (quarters, c25)    = centsInt `divMod` 25
        (dimes, c10)       = c25 `divMod` 10
        (nickles, pennies) = c10 `divMod` 5
    in showIfNoneZero hundreds " - One Hundred\n" ++
       showIfNoneZero twentys " - Twenty\n" ++
       showIfNoneZero tens " - Ten\n" ++
       showIfNoneZero fives " - Five\n" ++
       showIfNoneZero d5 " - One\n" ++
       showIfNoneZero quarters " - Quarter\n" ++
       showIfNoneZero dimes " - Dime\n" ++
       showIfNoneZero nickles " - Nickle\n" ++
       showIfNoneZero pennies " - Penny\n"

showIfNoneZero :: Int -> String -> String
showIfNoneZero n label
    | n /= 0    = show n ++ label
    | otherwise = ""