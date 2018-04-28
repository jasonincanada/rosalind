{- Rosalind

   Code:    PDST
   Problem: Creating a Distance Matrix
   URL:     http://rosalind.info/problems/pdst/
-}

import Bioinformatics (fastaSeq, parseFASTAdna)

type DNA = [Char]

-- Find the p-distance between two DNA strands, which is the percentage of
-- the strands that don't match
pdist :: DNA -> DNA -> Double
pdist a b = diff / len
  where diff = fromIntegral $ length $ filter (uncurry (/=)) $ zip a b
        len  = fromIntegral $ length a

process :: [DNA] -> [[Double]]
process dnas = [ [ pdist a b | b <- dnas ] | a <- dnas ]

main = do
  file <- readFile "pdst.input"
  let dnas = map fastaSeq $ parseFASTAdna file
  let matrix = process dnas
  mapM_ (putStrLn . unwords . map show) matrix

