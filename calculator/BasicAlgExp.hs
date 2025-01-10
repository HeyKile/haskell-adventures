module BasicAlgExp
  (checkChars,
   tokenizer,
   checkBalanced,
   infix2Postfix,
   evaluate) where

import Data.Char (isDigit)

checkChars :: [Char] -> [Char]
checkChars xs = case checkValidChars xs of
    Left err -> err
    Right _  -> xs

tokenizer :: [Char] -> [[Char]]
tokenizer [] = [[]]
tokenizer xs
  | checkError xs = [xs]
  | otherwise     = toTokens xs

checkBalanced :: [[Char]] -> [[Char]]
checkBalanced xs = case checkStrBalance (getDelimiters xs) [] of
    Left err -> err
    Right _  -> xs

infix2Postfix :: [[Char]] -> [[Char]]
infix2Postfix xs
  | checkError (head xs) = xs
  | otherwise            = toPostfix (toParen xs) [] []

evaluate :: [[Char]] -> [Char]
evaluate xs = if checkError (head xs) then head xs else case eval [] xs of
    Left err  -> err
    Right res -> res

{-
This is how the function calculate is defined in BasicCalculator.hs.

calculate :: [Char] -> [Char]
calculate = evaluate . infix2Postfix . checkBalanced . tokenizer . checkChars
-}

{-
  Solver functions for module
-}
checkValidChars :: [Char] -> Either [Char] [Char]
checkValidChars [] = Left "Error [checkChars]: The input str was empty"
checkValidChars xs = if all (`elem` validChars) xs
                      then Right xs
                      else Left ("Error [checkChars]: The character " ++ show (head (filter (`notElem` validChars) xs)) ++ " is not allowed")

toTokens :: [Char] -> [[Char]]
toTokens [] = []
toTokens (x:xs)
  | x `elem` nums = num : toTokens rest
  | x == ' '      = toTokens xs
  | otherwise     = [x] : toTokens xs
  where
    (num, rest) = span (`elem` nums) (x:xs)

checkStrBalance :: [[Char]] -> [[Char]] -> Either [[Char]] [[Char]]
checkStrBalance [] [] = Right []
checkStrBalance [] ys = Left ["Error [checkBalanced]: The expression is unbalanced"]
checkStrBalance (x:xs) []
  | x `elem` openChars = checkStrBalance xs [x]
  | otherwise          = Left ["Error [checkBalanced]: The expression is unbalanced"]
checkStrBalance (x:xs) (y:ys)
  | x `elem` openChars                  = checkStrBalance xs (x:y:ys)
  | x `elem` closeChars && x `closes` y = checkStrBalance xs ys
  | otherwise                           = Left ["Error [checkBalanced]: The expression is unbalanced"]

-- helper function for building postfixed exp
toPostfix :: [[Char]] -> [[Char]] -> [[Char]] -> [[Char]]
toPostfix [] ys zs = zs ++ ys
toPostfix (x:xs) ys zs
    | [head x] `elem` numsString = toPostfix xs ys (zs ++ [x])
    | x `elem` operators         = toPostfix xs opStack opOut
    | x `elem` openChars         = toPostfix xs (x:ys) zs
    | x == ")"                   = toPostfix xs clsStack clsOut
    | otherwise                  = toPostfix xs ys zs
    where
        isNumber = [head x] `elem` numsString
        (opStack, opOut) = popUntilLower x ys zs
        (clsStack, clsOut) = popUntilClose ys zs

-- pops elem off stack until top elem has lower precedence
popUntilLower :: [Char] -> [[Char]] -> [[Char]] -> ([[Char]], [[Char]])
popUntilLower x [] zs = ([x], zs)
popUntilLower x (y:ys) zs
    | x `hasLowerPrec` y && y /= "(" = popUntilLower x ys (zs ++ [y])
    | otherwise                      = (x:y:ys, zs)

-- pops elem off stack until close paren
popUntilClose :: [[Char]] -> [[Char]] -> ([[Char]], [[Char]])
popUntilClose [] zs = ([], zs)
popUntilClose (y:ys) zs
    | y == "("  = (ys, zs)
    | otherwise = popUntilClose ys (zs ++ [y])

-- evaluates a given expression
eval :: [[Char]] -> [[Char]] -> Either [Char] [Char]
eval stack []
  | length stack == 1 && isNumber (head stack) = Right (head stack)
  | length stack > 1  && isNumber (head stack) = Left "Error [evaluate]: too many operand(s)"
  | otherwise                                  = Left "Error [evaluate]: too many operator(s)"
eval (x:y:stack) (z:zs) = case z of
    "+" -> eval (show (read y + read x):stack) zs
    "-" -> eval (show (read y - read x):stack) zs
    "*" -> eval (show (read y * read x):stack) zs
    "/" -> case read x of
            0 -> Left "Error [evaluate]: divide by zero"
            _ -> eval (show (read y `div` read x):stack) zs
    _   -> eval (z:x:y:stack) zs
eval stack (x:xs) = eval (x:stack) xs

{-
  Helper functions
-}
isNumber :: [Char] -> Bool
isNumber ('-':xs) = all isDigit xs
isNumber xs       = all isDigit xs

closes :: [Char] -> [Char] -> Bool
closes x y
    | x == ")" && y == "(" = True
    | x == "]" && y == "[" = True
    | x == "}" && y == "{" = True
    | otherwise            = False

getDelimiters :: [[Char]] -> [[Char]]
getDelimiters = filter (`elem` delimiters)

prec :: [Char] -> Int
prec op
    | op `elem` highPrecOps = 2
    | op `elem` lowPrecOps  = 1
    | otherwise             = error "Invalid operator"

toParen :: [[Char]] -> [[Char]]
toParen = map (\x -> if x `elem` openChars then "(" else if x `elem` closeChars then ")" else x)

checkError :: [Char] -> Bool
checkError xs = take 5 xs == "Error"

-- checks precedence of x and y
hasLowerPrec :: [Char] -> [Char] -> Bool
hasLowerPrec x y
    | x `elem` highPrecOps && y `elem` lowPrecOps  = False
    | x `elem` lowPrecOps && y `elem` highPrecOps  = True
    | x `elem` highPrecOps && y `elem` highPrecOps = prec x <= prec y
    | x `elem` lowPrecOps && y `elem` lowPrecOps   = prec x <= prec y
    | otherwise                                    = False

-- helper lists
validChars :: [Char]
validChars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
                '+', '-', '*', '/', '(', ')', '[', ']', '{', '}',' ']

nums :: [Char]
nums = ['0'..'9']

numsString :: [[Char]]
numsString = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]

operators :: [[Char]]
operators = lowPrecOps ++ highPrecOps

highPrecOps :: [[Char]]
highPrecOps = ["/","*"]

lowPrecOps :: [[Char]]
lowPrecOps = ["-","+"]

delimiters :: [[Char]]
delimiters = openChars ++ closeChars

openChars :: [[Char]]
openChars = ["(", "[", "{"]

closeChars :: [[Char]]
closeChars = [")", "]", "}"]