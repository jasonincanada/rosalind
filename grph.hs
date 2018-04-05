{- Rosalind

   Code:    GRPH
   Problem: Overlap Graphs
   URL:     http://rosalind.info/problems/grph/
-}

import Data.List      (intercalate, isSuffixOf)
import Bioinformatics (FASTA(FASTA), fastaSeq, fastaID, parseFASTAdna)

data DiGraph a = DiGraph [(a, a)]

instance Show a => Show (DiGraph a) where
  show (DiGraph edges) = intercalate "\n" $ map (\(s, t) -> show s ++ " " ++ show t) edges

process :: Int -> [FASTA] -> DiGraph String
process k fs = DiGraph [ (fastaID s, fastaID t) 
                         | s <- fs,
                           t <- fs,
                           fastaSeq s /= fastaSeq t,
                           isSuffixOf (take k $ fastaSeq t) (fastaSeq s)
                       ]

main = do
  file <- readFile "grph.input"
  let parsed = parseFASTAdna file
  print $ process 3 parsed

