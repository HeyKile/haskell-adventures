-- Kyle Bello
-- Functional Programming Assignment 6
-- Huffman Encoder / Decoder

module HuffmanCoding (toHuffmanCode, fromHuffmanCode) where

import BTree
import Data.Function (on)
import Data.List (sortBy)
import Data.Set ( fromList, toList )

-- ==================== Module ====================

data HBit = L | R deriving (Show, Eq, Read)

-- encoding function
toHuffmanCode :: [Char] -> ([Char], [Char])
toHuffmanCode str = (treeStr, hBitStr)
    where
        tuples = getFrequencyTuples str
        encoding = huffmanEncode (toBTrees tuples)
        hBit = toHBit encoding str
        treeStr = show encoding
        hBitStr = show hBit

-- decoding function
fromHuffmanCode :: [Char] -> [Char] -> [Char]
fromHuffmanCode [] [] = error "Empty inputs"
fromHuffmanCode [] _  = error "Empty huffman code"
fromHuffmanCode _ []  = error "Empty huffman tree"
fromHuffmanCode treeStr codeStr = huffmanDecode tree code
    where
        tree = read treeStr :: BTree (Char, Int)
        code = read codeStr :: [HBit]

-- ==================== Encoding ====================

-- helper encode function, ensures onlt 1 tree is returned
huffmanEncode :: [BTree (Char, Int)] -> BTree (Char, Int)
huffmanEncode [] = error "Empty list"
huffmanEncode trees
    | length res /= 1 = error "Encoding fail, length of list /= 1"
    | otherwise       = head res
    where
        res = buildEncodedBTree trees

-- combines a sorted list of BTrees into 1 BTree
buildEncodedBTree :: [BTree (Char, Int)] -> [BTree (Char, Int)]
buildEncodedBTree [] = []
buildEncodedBTree [ys] = [ys]
buildEncodedBTree (x:y:ys) = buildEncodedBTree sortedTrees
    where
        sum = sumBTreeCounts x y
        newTree = BTree (nul, sum) x y
        sortedTrees = sortBTreesByFreq (newTree:ys)

-- Uses a given huffman tree and str to build a huffman code
toHBit :: BTree (Char, Int) -> [Char] -> [HBit]
toHBit EmptyBTree _ = []
toHBit _ [] = []
toHBit tree (x:xs) = findChars tree x ++ toHBit tree xs

-- helper function for building huffman code
findChars :: BTree (Char, Int) -> Char -> [HBit]
findChars EmptyBTree _ = []
findChars tree char
    | not resBool = error "character not in tree"
    | otherwise   = resHBit
    where
        (resHBit, resBool) = findChar tree char False []

-- recursively searches BTree for given char
findChar :: BTree (Char, Int) -> Char -> Bool -> [HBit]-> ([HBit], Bool)
findChar EmptyBTree _ _ _ = ([], False)
findChar (BTree (cur, _) EmptyBTree EmptyBTree) char found curHBit
    | cur == char = (curHBit, True)
    | otherwise   = (curHBit, False)
findChar (BTree (nul, _) left right) char found curHBit
    | rFound = (rHBit, rFound)
    | lFound = (lHBit, lFound)
    | otherwise = (curHBit, False)
    where
        (rHBit, rFound) = findChar right char found (curHBit ++ [R])
        (lHBit, lFound) = findChar left char found (curHBit ++ [L])

-- ==================== Decode ====================

-- helper decode function w/ parsed tree & huffman code
huffmanDecode :: BTree (Char, Int) -> [HBit] -> [Char]
huffmanDecode EmptyBTree _ = error "Empty huffman tree"
huffmanDecode _ []         = error "Empty huffman code"
huffmanDecode tree code = buildStr tree tree code

-- builds the string from a given huffman tree and huffman code
buildStr :: BTree (Char, Int) -> BTree (Char, Int) -> [HBit] -> [Char]
buildStr _ EmptyBTree _ = []
buildStr EmptyBTree _ _ = []
buildStr _ (BTree (cur, freq) EmptyBTree EmptyBTree) [] = [cur]
buildStr root (BTree (cur, freq) EmptyBTree EmptyBTree) xs = cur : buildStr root root xs
buildStr root (BTree _ left right) (x:xs) =
    case x of
        L -> buildStr root left xs
        R -> buildStr root right xs

-- ==================== Helper Functions ====================

-- generates tuples from an str for each char & its frequency
getFrequencyTuples :: String -> [(Char, Int)]
getFrequencyTuples "" = [('\NUL', 0)]
getFrequencyTuples str = counts
    where
        chars = toList (fromList str)
        counts = sortTupByFreq [(cur, freq) | cur <- chars, let freq = length (Prelude.filter (== cur) str), freq /= 0]

-- turns list of frequency tuples into list of BTrees
toBTrees :: [(Char, Int)] -> [BTree (Char, Int)]
toBTrees [] = []
toBTrees (x:xs) = sortBTreesByFreq (BTree x EmptyBTree EmptyBTree : toBTrees xs)

-- combines 2 tuples BTrees into 1 BTree
makeBTree :: (Char, Int) -> (Char, Int) -> BTree (Char, Int)
makeBTree (l1, c1) (l2, c2) = BTree (nul, sum) (BTree (l1, c1) EmptyBTree EmptyBTree) (BTree (l2, c2) EmptyBTree EmptyBTree)
    where
        nul = '\NUL'
        sum = c1 + c2

-- sums the frequencies of 2 BTrees
sumBTreeCounts :: BTree (Char, Int) -> BTree (Char, Int) -> Int
sumBTreeCounts t1 t2 = getFrequency t1 + getFrequency t2

getFrequency :: BTree (Char, Int) -> Int
getFrequency EmptyBTree = 0
getFrequency (BTree (_, freq) _ _) = freq

-- shorthand for null char
nul :: Char
nul = '\NUL'

-- ==================== Sorting ====================

-- sorts list of tuples by freqency
sortTupByFreq :: [(Char, Int)] -> [(Char, Int)]
sortTupByFreq [] = []
sortTupByFreq xs = sortBy (compare `on` snd) xs

-- sorts list of BTrees by their freq
sortBTreesByFreq :: [BTree (Char, Int)] -> [BTree (Char, Int)]
sortBTreesByFreq = sortBy (compare `on` getFrequency)


-- func = do
--     -- putStrLn (showBTree (read cheese :: BTree (Char, Int)))
--     putStrLn cheese
--     putStrLn (show (read crackers :: [HBit]))
--     putStrLn "Your decode message is: "
--     putStrLn decoded
--     where 
--         (cheese, crackers) = toHuffmanCode "I live for the thrill, and thrill for the live. Yeah, I'm chill like that."
--         decoded = fromHuffmanCode cheese crackers