{- Rosalind

   Code:    SSEQ
   Problem: Finding a Spliced Motif
   URL:     http://rosalind.info/problems/sseq/
-}

import Bioinformatics (fastaSeq, parseFASTAdna)

-- Return (not necessarily contiguous) indices of the characters of t in s
process :: String -> String -> [Int]
process = go 1
  where go _ _ [] = []
        go _ [] _ = error "String t not fully represented in s"
        go i (s:ss) (t:ts)
          | s == t    = i : go (i+1) ss ts
          | otherwise =     go (i+1) ss (t:ts) 

main = do
  file <- readFile "sseq.input"
  let parsed = map fastaSeq $ parseFASTAdna file
  let s = parsed !! 0
  let t = parsed !! 1
  let indices = process s t
  putStrLn $ unwords $ map show indices

