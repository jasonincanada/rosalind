{- Rosalind

   Code:    PMCH
   Problem: Perfect Matchings and RNA Secondary Structures
   URL:     http://rosalind.info/problems/pmch/
-}

import Bioinformatics (fastaSeq, parseFASTArna)

type RNA = [Char]

count :: RNA -> (Int, Int)
count rna = let a = length $ filter (=='A') rna
                c = length $ filter (=='C') rna
            in  (a, c)

fac :: Integer -> Integer
fac 1 = 1
fac n = n * fac (n-1)

main = do
  file <- readFile "pmch.input"
  let rna = fastaSeq $ head $ parseFASTArna file
  let (a, c) = count rna
  print $ fac (toInteger a) * fac (toInteger c)

