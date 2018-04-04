{- Rosalind

   Code:    GRPH
   Problem: Overlap Graphs
   URL:     http://rosalind.info/problems/grph/
-}

import Data.List      (intercalate, isSuffixOf)
import Bioinformatics (FASTA(FASTA), fastaDNA, fastaID, parseFASTA)

data DiGraph a = DiGraph [(a, a)]

instance Show a => Show (DiGraph a) where
  show (DiGraph edges) = intercalate "\n" $ map (\(s, t) -> show s ++ " " ++ show t) edges

process :: Int -> [FASTA] -> DiGraph String
process k fs = DiGraph [ (fastaID s, fastaID t) 
                         | s <- fs,
                           t <- fs,
                           fastaDNA s /= fastaDNA t,
                           isSuffixOf (take k $ fastaDNA t) (fastaDNA s)
                       ]

main = do
  file <- readFile "grph.input"
  let parsed = parseFASTA file
  print $ process 3 parsed

