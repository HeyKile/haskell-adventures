module EitherAlgExp
  (eitherCheckChars,
   eitherTokenizer,
   eitherCheckBalanced,
   eitherInfix2Postfix,
   eitherEvaluate) where

import Data.Char (isDigit)

eitherCheckChars :: [Char] -> Either [Char] [Char]
eitherCheckChars [] = Left "Error [checkChars]: The input str was empty"
eitherCheckChars xs = if all (`elem` validChars) xs
                        then Right xs
                        else Left ("Error [checkChars]: The character " ++ show (head (filter (`notElem` validChars) xs)) ++ " is not allowed")

eitherTokenizer :: [Char] -> Either [Char] [[Char]]
eitherTokenizer xs
  | checkError xs = Left xs
  | otherwise     = Right (toTokens xs)

eitherCheckBalanced :: [[Char]] -> Either [Char] [[Char]]
eitherCheckBalanced [] = Left "Error [checkBalanced]: The input list was empty"
eitherCheckBalanced (x:xs)
  | checkError x = Left x
  | otherwise    = case checkStrBalance (getDelimiters (x:xs)) [] of
                    Left err -> Left err
                    Right _  -> Right (x:xs)

eitherInfix2Postfix :: [[Char]] -> Either [Char] [[Char]]
eitherInfix2Postfix [] = Left "Error [eitherInfix2Postfix]: The input list was empty"
eitherInfix2Postfix (x:xs)
  | checkError x = Left x
  | otherwise    = Right (toPostfix (toParen (x:xs)) [] [])

eitherEvaluate :: [[Char]] -> Either [Char] [Char]
eitherEvaluate (x:xs)
  | checkError x = Left x
  | otherwise    = eval [] (x:xs)

{-
--This is how the function eitherCalculate is defined in BasicCalculator.hs.

eitherCalculate :: [Char] -> Either [Char] [Char]
eitherCalculate str = pure str >>=
                      eitherCheckChars >>=
                      eitherTokenizer >>=
                      eitherCheckBalanced >>=
                      eitherInfix2Postfix >>=
                      eitherEvaluate
-}

-- checks if given chars are valid
checkValidChars :: [Char] -> Either [Char] [Char]
checkValidChars [] = Left "Error [checkChars]: The input str was empty"
checkValidChars xs = if all (`elem` validChars) xs
                      then Right xs
                      else Left ("Error [checkChars]: The character " ++ show (head (filter (`notElem` validChars) xs)) ++ " is not allowed")

-- turns input str into tokens
toTokens :: [Char] -> [[Char]]
toTokens [] = []
toTokens (x:xs)
  | x `elem` nums = num : toTokens rest
  | x == ' '      = toTokens xs
  | otherwise     = [x] : toTokens xs
  where
    (num, rest) = span (`elem` nums) (x:xs)

-- checks if delimiters from a given str are balanced
-- note: input must be list of just delimiters
checkStrBalance :: [[Char]] -> [[Char]] -> Either [Char] [[Char]]
checkStrBalance [] [] = Right []
checkStrBalance [] ys = Left "Error [checkBalanced]: The expression is unbalanced"
checkStrBalance (x:xs) []
  | x `elem` openChars = checkStrBalance xs [x]
  | otherwise          = Left "Error [checkBalanced]: The expression is unbalanced"
checkStrBalance (x:xs) (y:ys)
  | x `elem` openChars                  = checkStrBalance xs (x:y:ys)
  | x `elem` closeChars && x `closes` y = checkStrBalance xs ys
  | otherwise                           = Left "Error [checkBalanced]: The expression is unbalanced"

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

-- helper function for operator precedence
prec :: [Char] -> Int
prec op
    | op `elem` highPrecOps = 2
    | op `elem` lowPrecOps  = 1
    | otherwise             = error "Invalid operator"

-- turns all open/close chars into parentheses for convenience
toParen :: [[Char]] -> [[Char]]
toParen = map (\x -> if x `elem` openChars then "(" else if x `elem` closeChars then ")" else x)

-- checks precedence of x and y
hasLowerPrec :: [Char] -> [Char] -> Bool
hasLowerPrec x y
    | x `elem` highPrecOps && y `elem` lowPrecOps  = False
    | x `elem` lowPrecOps && y `elem` highPrecOps  = True
    | x `elem` highPrecOps && y `elem` highPrecOps = prec x <= prec y
    | x `elem` lowPrecOps && y `elem` lowPrecOps   = prec x <= prec y
    | otherwise                                    = False

checkError :: [Char] -> Bool
checkError xs = take 5 xs == "Error"

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