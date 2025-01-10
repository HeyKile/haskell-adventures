-- BTree data type
-- for use with huffman encoding/decoding algorithm

module BTree (BTree (..), getRootData, getHeight, getNumberOfNodes, showBTree) where

-- ========== Data Types ==========

data BTree a = EmptyBTree | BTree a (BTree a) (BTree a) deriving (Show, Eq, Read)

-- ========== Module Functions ==========

getRootData :: BTree a -> a
getRootData EmptyBTree = error "no root data"
getRootData (BTree root left right) = root

getHeight :: BTree a -> Int
getHeight EmptyBTree = 0
getHeight (BTree root left right) = max (1 + getHeight left) (1 + getHeight right)

getNumberOfNodes :: BTree a -> Int
getNumberOfNodes EmptyBTree = 0
getNumberOfNodes (BTree root left right) = 1 + getNumberOfNodes left + getNumberOfNodes right

showBTree :: Show a => BTree a -> String
showBTree EmptyBTree = ""
showBTree (BTree cur left right) = unlines ([show cur] ++ buildSubTree right [True] ++ buildSubTree left [False])

-- ========== Helper Functions ==========

-- builds the string of a given subtree
buildSubTree :: Show a => BTree a -> [Bool] -> [String]
buildSubTree EmptyBTree _ = []
buildSubTree (BTree cur EmptyBTree EmptyBTree) padding = [pad padding ++ showNode cur]
buildSubTree (BTree cur left right) padding = [pad padding ++ showNode cur] ++ rightStr ++ leftStr
    where
        newPadding = padding ++ [True]
        rightStr
            | isEmptyBTree right = [pad newPadding ++ "+"]
            | otherwise          = buildSubTree right newPadding
        leftEmpty = isEmptyBTree left
        leftStr
            | leftEmpty = [pad newPadding ++ "+"]
            | otherwise         = buildSubTree left (padding ++ [False])

-- pads a subtree string based on [bool] (True == move right / "| ", False == move left / "  ")
pad :: [Bool] -> String
pad [] = ""
pad (x:xs)
    | length (x:xs) == 1 = ""
    | x = "| " ++ pad xs
    | otherwise = "  " ++ pad xs

-- checks if a given BTRee is empty
isEmptyBTree :: BTree a -> Bool
isEmptyBTree EmptyBTree = True
isEmptyBTree BTree {} = False

-- shows node in printable format
showNode :: Show a => a -> String
showNode str
    | show str == "" = "+"
    | otherwise      = "+--" ++ show str