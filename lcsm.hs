{- Rosalind

   Code:    LCSM
   Problem: Finding a shared motif
   URL:     http://rosalind.info/problems/lcsm/
-}

import Bioinformatics (fastaSeq, parseFASTAdna)
import Data.List      (isInfixOf)

-- All substrings of a string in descending order of size
substrings :: Eq a => [a] -> [[a]]
substrings [] = []
substrings s  = go (length s) s
  where go 0 _ = []
        go n s = allSubs n s ++ go (n-1) s

-- This function is from: https://stackoverflow.com/questions/27523398/creating-a-list-of-substrings-of-a-specified-length-in-haskell
allSubs :: Int -> [a] -> [[a]]
allSubs n s
    | length s >= n = take n s : allSubs n (tail s)
    | otherwise = []

-- Return the largest sublist common to a list of lists
largest :: Eq a => [[a]] -> [a]
largest [] = []
largest as = let candidates = substrings $ head as
             in  head [ s | s <- candidates,
                            all (isInfixOf s) as ]

main = do
  file <- readFile "lcsm.input"
  let strands = map fastaSeq $ parseFASTAdna file
  putStrLn $ largest strands

