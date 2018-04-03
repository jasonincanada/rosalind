{- Rosalind

   Code:    GC
   Problem: Computing GC Content
   URL:     http://rosalind.info/problems/gc/
-}

import Data.List           (maximumBy)
import Data.Ord            (comparing)
import Control.Applicative (many, some)
import Bioinformatics      (FASTA(FASTA), fastaID, parseFASTA)
import NanoParsec

-- Return the percentage of a DNA string that consists of C or G nucleotides
gcContent :: String -> Float
gcContent [] = 0
gcContent ns = num / denom
  where num   = fromIntegral $ length . filter (flip elem "CG") $ ns
        denom = fromIntegral $ length ns

gcpct :: FASTA -> Float
gcpct (FASTA _ dna) = gcContent dna

main = do
  file <- readFile "gc.input"
  let parsed = parseFASTA file
  let max = maximumBy (comparing gcpct) parsed
  print $ (fastaID max, 100 * gcpct max)

