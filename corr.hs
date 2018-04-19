{- Rosalind

   Code:    CORR
   Problem: Error correction in reads
   URL:     http://rosalind.info/problems/corr/
-}

import Bioinformatics (fastaSeq, parseFASTAdna)
import Data.List      (nub)

type DNA = String

complement :: DNA -> DNA
complement = map c
  where c 'A' = 'T'
        c 'T' = 'A'
        c 'G' = 'C'
        c 'C' = 'G'

rc :: String -> String
rc = reverse . complement

-- Count number of nucleotide differences between two DNA strands.
-- Note: This assumes the strands are equal length, which is guaranteed 
-- in the description for this particular challenge
hamming :: DNA -> DNA -> Int
hamming a b = length . filter (uncurry (/=)) $ zip a b

process :: [DNA] -> [(DNA, DNA)]
process dnas = nub $ [ (d1, d2) | d1 <- dnas, 
                                  d2 <- dnas,
                                  hamming d1 d2 == 1,
                                  count d2 >= 2 ]
               ++ [ (d1, rc d2) | d1 <- dnas, 
                                  d2 <- dnas,
                                  hamming d1 (rc d2) == 1,
                                  count d2 >= 2 ]

  where 
    -- Count the number of times we see this strand or its reverse complement
    count :: DNA -> Int
    count dna = length $ filter (\d -> d == dna || rc d == dna) dnas

main = do
  file <- readFile "corr.input"
  let strands = map fastaSeq $ parseFASTAdna file
  let tuples = process strands
  mapM_ putStrLn $ map (\(a,b) -> a ++ "->" ++ b) tuples
 
